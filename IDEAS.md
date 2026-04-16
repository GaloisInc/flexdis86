# Deferred optimization ideas for `DefaultParser.hs`

These were considered but deferred in favor of the current approach
(streaming Binary serialization during the TH splice).

---

## 1. `BytesPrimL` instead of `StringPrimL`

`StringPrimL [Word8]` generates an `Addr#` literal in `.rodata`.
`BytesPrimL (Bytes ptr 0 len)` generates a `ByteArray#` literal in `.rodata`.

`BytesPrimL` avoids the null-terminator byte appended by `StringPrimL` and
can be paired with `unsafePackByteArray` directly, skipping the
`unsafePerformIO (unsafePackAddressLen n addr#)` indirection.

The runtime overhead of the current `unsafePerformIO` + `unsafePackAddressLen`
call is negligible (it only runs once, at `optableBytes` first evaluation).
Switch to `BytesPrimL` only if GHC ever starts inlining / duplicating the CAF
despite `{-# NOINLINE #-}`.

---

## 2. Custom `Binary Def` to shrink the blob

The `_defPrefix :: [String]` field is encoded naively: each `Char` costs 8
bytes under the generic `Binary Char` instance (via `Binary Int32`).
A custom `putByteString`-based instance would shrink the blob by ~70 KB
(from ~420 KB to ~350 KB) and speed up decoding.

Worth doing if object-file size or first-use latency become a concern.

---

## 3. Split key/value encoding

Instead of one flat `[Def]` list, split into two parallel arrays:

* `keyBlob   :: ByteString` — opcodes and other lookup-key fields
* `valueBlob :: ByteString` — mnemonic strings, operand lists, etc.

The key blob is accessed on every disassembly; the value blob only when
constructing user-visible `InstructionInstance` values.  Keeping them
separate lets the OS page-cache focus on the hot key data.

---

## 4. zlib/zstd compression

The blob is currently ~420 KB and compresses to ~250 KB (zstd level 1).
Embedding the compressed form and decompressing once at startup would reduce
the `.o` file size.  Trade-off: adds a runtime decompression step and a new
dependency (`zlib` or `zstd`).  Probably not worth it unless the blob grows
significantly.

---

## 5. Flat struct-of-arrays layout

Replace the list of records with a flat binary layout:

```
Word64  count
count × fixed-size header (CPU req, flags, opcode bytes)
variable-length section (mnemonics, operand lists)
```

This would let `mkOpcodeTable` walk the header section with zero allocation
and do pointer arithmetic instead of list traversal.  Complex to implement;
revisit if profiling shows `mkOpcodeTable` is a bottleneck.
