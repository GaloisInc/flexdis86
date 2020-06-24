-- | Test that the result of assembling an ASM string using GCC is
-- unchanged when we disassemble and reassembling it using Flexdis86.
--
-- Note that ASM below is in AT&T syntax, but Intel syntax is also
-- supported by the underlying 'withAssembledCode'.
module Roundtrip ( roundtripTests ) where

import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe ( mapMaybe )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Flexdis86 as D
import           Hexdump

import           Util ( AsmFlavor(..), withAssembledCode )

roundtripTests :: T.TestTree
roundtripTests = T.testGroup "Roundtrip Tests"
  [ zeroOperandTests
  , immediateTests
  , singleOperandTests
  , twoOperandTests
  , addressingModeTests
  , mmxTests
  , sseTests
  ]

zeroOperandTests :: T.TestTree
zeroOperandTests =
  T.testGroup "ZeroOperandOpcodes" (map mkTest zeroOperandOpcodeTests)

zeroOperandOpcodeTests :: [(String, [String])]
zeroOperandOpcodeTests = [ ("ret", ["ret"])
                         , ("endbr32", ["endbr32"])
                         , ("endbr64", ["endbr64"])
                         , ("x87 wait", ["wait"])
                         , ("int3", ["int $0x3"])
                           -- FIXME: This gets rendered as xor eax,eax
                           --
                           -- That is probably valid, but we can't
                           -- re-produce the 1-byte no

--                         , ("nop", ["nop"])
                         , ("halt", ["hlt"])
                         , ("cmc", ["cmc"])
                         , ("clear carry flag", ["clc"])
                         , ("set carry flag", ["stc"])
                         , ("clear interrupt flag", ["cli"])
                         , ("set interrupt flag", ["sti"])
                         , ("clear direction flag", ["cld"])
                         , ("set direction flag", ["std"])
                         , ("undefined instruction", ["ud2"])
                         , ("memory fence", ["mfence"])
                         , ("store fence", ["sfence"])
                         , ("load fence", ["lfence"])
                           -- This test (cwd) requires a size override
                           -- to 16 bits because it otherwise shares
                           -- an opcode with cdq
                         , ("convert word to dword", ["cwd"])
                         , ("convert dword to qword", ["cdq"])
                         , ("swapgs", ["swapgs"])
                         , ("xgetbv", ["xgetbv"])
                         , ("Empty MMX state", ["emms"])
                         , ("Read performance counters", ["rdpmc"])
                         , ("Read model-specific register", ["rdmsr"])
                         , ("push lower EFLAGS", ["pushf"])
                         , ("push rflags", ["pushfq"])
                         , ("pause", ["pause"])
                         , ("outsb", ["outsb"])
                         , ("outsw", ["outsw"])
                         , ("convert byte to word", ["cbw"])
                         , ("convert byte to dword", ["cwde"])
                         , ("convert byte to qword", ["cdqe"])
                         , ("ExamineModR/M", ["fxam"])
                         ]

singleOperandTests :: T.TestTree
singleOperandTests =
  T.testGroup "SingleOperandOpcodes" (map mkTest singleOperandOpcodes)

singleOperandOpcodes :: [(String, [String])]
singleOperandOpcodes = [ ("increment r8/ah", ["inc %ah"])
                         -- %al is 0, while %ah is non-zero
                       , ("increment r8/al", ["inc %al"])
                       , ("increment r16/ax", ["inc %ax"])
                       , ("increment r16/bx", ["inc %bx"])
                       , ("increment r32", ["inc %eax"])
                       , ("increment r64", ["inc %rax"])
                         -- %edx is interesting because the encoding
                         -- of %eax is 0, while %edx is non-zero.
                         -- This will make sure we shift the REG field
                         -- correctly.
                       , ("increment edx", ["inc %edx"])
                       , ("atomic increment mem32 via edx", ["lock incl (%edx)"])
                       , ("atomic increment mem32 via rcx", ["lock incl (%rcx)"])
                       , ("atomic increment mem32 via r12", ["lock incl (%r12)"])
                       , ("atomic increment mem16 via r12", ["lock incw (%r12)"])
                       , ("atomic increment mem32 via r9", ["lock incl (%r9)"])
                       , ("atomic increment mem8 via eax", ["lock incb (%eax)"])
                         -- This one uses a memory reference through a register
                       , ("increment mem32 via eax", ["incl (%eax)"])
                       , ("increment mem32 via ecx", ["incl (%ecx)"])
                       , ("increment mem32 via esi", ["incl (%esi)"])
                       , ("increment mem32 via edi", ["incl (%edi)"])
                       , ("increment mem16 via eax", ["incw (%eax)"])
                       , ("increment mem16 via edx", ["incw (%edx)"])
                       , ("increment mem8 via eax", ["incb (%eax)"])
                       , ("increment mem8 via ebx", ["incb (%ebx)"])
                       , ("increment mem64 via eax", ["incq (%eax)"])
                       , ("increment mem64 via ecx", ["incq (%ecx)"])
                         -- Memory reference through a 64 bit register
                       , ("increment mem32 via rax", ["incl (%rax)"])
                       , ("increment mem32 via rdx", ["incl (%rdx)"])
                       , ("increment mem8 via r12", ["incb (%r12)"])
                       , ("increment mem8 via r13", ["incb (%r13)"])
                         -- Reg+disp8
                       , ("increment mem32 via edx+disp8", ["incl 0x10(%edx)"])
                       , ("increment mem32 via edx+disp32", ["incl 0x1000000(%edx)"])
                       , ("increment mem32 via rdx+disp8", ["incl 0x10(%rdx)"])
                       , ("increment mem32 via rdx+disp32", ["incl 0x1000000(%rdx)"])
                       , ("Divide (ax) by r8 (bl)", ["div %bl"])
                       , ("Divide (ax) by r16 (bx)", ["div %bx"])
                       , ("Divide (ax) by r32 (ebx)", ["div %ebx"])
                       , ("Divide (ax) by r64 (rbx)", ["div %rbx"])
                       , ("bswap 32", ["bswap %ebx"])
                       , ("bswap 64", ["bswap %r8"])
                       , ("bswap 64ext", ["bswap %r12"])
                       , ("jmp rel8", ["jmp .+0x8"])
                       , ("jmp rel32", ["jmp .+0xfff"])
                       , ("jmp (%ebx)", ["jmp *(%ebx)"])
                       , ("jc rel8", ["jc .+0x8"])
                       , ("loop rel8", ["loop .+0x8"])
                       , ("pop %rbp", ["pop %rbp"])
                       , ("push %rbp", ["push %rbp"])
                       , ("sete %al", ["sete %al"])
                       , ("shr %rdx", ["shr %rdx"])
                       , ("shl    %cl,%rax", ["shl    %cl,%rax"])
                       , ("shr %cl,%rax", ["shr %cl, %rax"])
                       , ("nopw %cs:0x0(%rax,%rax,1)", ["nopw %cs:0x0(%rax,%rax,1)"])
                       , ("rep stos %rax,%es:(%rdi)", ["rep stos %rax,%es:(%rdi)"])
                       , ("jmpq   *0x4096a0(,%rax,8)", ["jmpq   *0x4096a0(,%rax,8)"])
                       , ("nopl (%rax)", ["nopl (%rax)"])
                       , ("fldt   0x1d90(%rsp)", ["fldt   0x1d90(%rsp)"])
                       ]

addressingModeTests :: T.TestTree
addressingModeTests = T.testGroup "AddressingModeTests" (map mkTest addressingModes)

addressingModes :: [(String, [String])]
addressingModes = [ ("DirectReg r/m", ["and %ecx, %esi"])
                  , ("NoDisplacement [r/m] (mem-reg)", ["and %ecx, (%edx)"])
                  , ("NoDisplacement [SIB]", ["and %edx,(%esp)"])
                  , ("NoDisplacement [RIP/EIP + disp32]", ["and %ecx, 0x5555(%rip)"])
                  , ("NoDisplacement [r/m] 2", ["and %rcx,(%r9)"])
                  , ("NoDisplacement [SIB] r12", ["and %rcx,(%r12)"])
                  , ("NoDisplacement [RIP/EIP + disp32] r13", ["and %rcx,(%r13)"])
                  , ("NoDisplacement [r/m] 3", ["and %rdx,(%r14)"])
                  , ("Disp8 [r/m+disp8]", ["and %rbx,0x7(%rcx)"])
                  , ("Disp8 [SIB+disp8] SP", ["and %rbx,0x7(%rsp)"])
                  , ("Disp8 [SIB+disp8] 1", ["and %rbx,0x7(%rcx,%rcx,1)"])
                  , ("Disp8 [SIB+disp8] 2", ["and %rbx,0x7(%rcx,%rcx,2)"])
                  , ("Disp8 [SIB+disp8] 4", ["and %rbx,0x7(%rcx,%rcx,4)"])
                  , ("Disp8 [SIB+disp8] 8", ["and %rbx,0x7(%rcx,%rcx,8)"])
                  , ("Disp8 [SIB+disp8] r12", ["and %rbx,0x7(%r12)"])
                  , ("Disp32 [r/m+disp32]", ["and %rbx,0x777(%rcx)"])
                  , ("Disp32 [SIB+disp32] SP", ["and %rbx,0x777(%rsp)"])
                  , ("Disp32 [SIB+disp32] 1", ["and %rbx,0x777(%rcx,%rcx,1)"])
                  , ("Disp32 [SIB+disp32] r12", ["and %rbx,0x777(%r12)"])
                  , ("reg-reg", ["and %ecx, %esi"])
                  , ("[mem]-reg", ["and %edx,(%ebp)"])
                  , ("[mem+disp8]-reg", ["and 0x8(%edx),%ebx"])
                  , ("[mem+disp8]-reg SIB", ["and (%ecx, %ecx, 1),%ebx"])
                  , ("[mem+disp8]-reg SIB (non-zero offset)", ["and 0x9(%ecx, %ecx, 1),%ebx"])
                  , ("[mem+disp32]-reg", ["and 0x1234(%edx),%ebx"])
                  , ("[mem+disp32]-reg SIB/2", ["and 0x1234(%edx, %edx, 2),%ebx"])
                  , ("[mem+disp32]-reg SIB/4", ["and 0x1234(%edx, %edx, 4),%ebx"])
                  , ("[mem+disp32]-reg SIB/8", ["and 0x1234(%edx, %edx, 8),%ebx"])
                  , ("rip-relative", ["and %ecx,0x5555(%rip)"])
                  ]

twoOperandTests :: T.TestTree
twoOperandTests =
  T.testGroup "TwoOperandTests" (map mkTest twoOperandOpcodes)

twoOperandOpcodes :: [(String, [String])]
twoOperandOpcodes = [ ("test reg reg (eax)", ["test %eax, %eax"])
                    , ("test reg reg (edx)", ["test %edx, %edx"])
                    , ("test reg reg (ebx, ecx)", ["test %ebx, %ecx"])
                    , ("test al imm8", ["test $1, %al"])
--                    , ("test ax imm16", ["test $12, %ax"])
                    , ("test eax imm32", ["test $12, %eax"])
--                    , ("test rax imm32", ["test $12, %rax"])
                    , ("mov r8, r8", ["mov %al, %bl"])
                    , ("mov r8, imm8", ["mov $8, %bl"])
                    , ("mov r64, imm64", ["mov $10000000000, %r9"])
                    , ("mov (%rdi),%r14b", ["mov (%rdi),%r14b"])
                    , ("mov extended r64, imm64", ["mov $10000000000, %r13"])
                    , ("movq imm with off", ["movq $0xffffffffffffffff,-0x8(%rbp)"])
                    , ("add 1, eax", ["add $0x1, %eax"])
                    , ("movl 0, -0x14(%rbp)", ["movl $0x0,-0x14(%rbp)"])
                    , ("sub    $0x28,%rsp", ["sub $0x28,%rsp"])
                    , ("sub    (%rax),%rsp", ["sub (%rax),%rsp"])
                    , ("sub    %rsp,(%rax)", ["sub %rsp,(%rax)"])
                    , ("lea    (%rax,%rax,1),%ebx", ["lea (%rax,%rax,1),%ebx"])
                    , ("lods   %ds:(%rsi),%al", ["lods %ds:(%rsi),%al"])
                    , ("movl   $0x0,-0x4(%rbp)", ["movl   $0x0,-0x4(%rbp)"])
                    , ("mov    -0x4(%rbp),%eax", ["mov    -0x4(%rbp),%eax"])
                    , ("sub    $0x41,(%rdx, %rax, 3)", ["sub    $0x41,(%rdx, %rax, 2)"])
                    , ("lea    0xe0(%rsp),%rax", ["lea    0xe0(%rsp),%rax"])
                    , ("test %al,%al", ["test %al,%al"])
                    , ("mov %rsp,%rbp", ["mov %rsp,%rbp"])
                    , ("movzbl -0x1(%rbp),%eax", ["movzbl -0x1(%rbp),%eax"])
                    , ("movsbl -0x1(%rbp),%eax", ["movsbl -0x1(%rbp),%eax"])
                    , ("movq   $0xffffffffffffffff,-0x10(%rbp)", ["movq   $0xffffffffffffffff,-0x10(%rbp)"])
                    , ("movl   $0xffffffff,-0xc(%rbp)", ["movl   $0xffffffff,-0xc(%rbp)"])
                    , ("mov    -0xc(%rbp),%eax", ["mov    -0xc(%rbp),%eax"])
                    , ("mov %al,-0x1(%rbp)", ["mov %al,-0x1(%rbp)"])
                    , ("cmpl $0x1fe,-0x8(%rbp)", ["cmpl $0x1fe,-0x8(%rbp)"])
                    , ("add $0x3,%eax", ["add $0x3,%eax"])
                    , ("mov %rsp,%rbp", ["mov %rsp,%rbp"])
                    , ("mov %edx,%eax", ["mov %edx,%eax"])
                    , ("movw   $0xffff,-0x4(%rbp)", ["movw   $0xffff,-0x4(%rbp)"])
                    , ("movb   $0xff,-0x1(%rbp)", ["movb   $0xff,-0x1(%rbp)"])
                    , ("movsxd %edi, %rax", ["movsxd %edi, %rax"])
                    , ("movzx %al, %eax", ["movzx %al, %eax"])
                    , ("cmp $0x0,(%rsi,%rax,8)", ["cmp $0x0,(%rsi,%rax,8)"])
                    -- This movabs variant is interesting because the eax/rax
                    -- destinations have a special encoding
                    , ("movabs 0xe000ed04,%eax", ["movabs 0xe000ed04,%eax"])
                    , ("movabs 0xe000ed04,%rax", ["movabs 0xe000ed04,%rax"])
                    -- This is a normal movabs
                    , ("movabs $0xaaaaaaaaaaaaaaa9,%rdx", ["movabs $0xaaaaaaaaaaaaaaa9,%rdx"])
                    , ("test %rdi,%rdi", ["test %rdi,%rdi"])
                    , ("mov %fs:0x28,%rax", ["mov %fs:0x28,%rax"])
                    , ("mov $0x1,%esi", ["mov $0x1,%esi"])
                    , ("repz cmpsb %es:(%rdi),%ds:(%rsi)", ["repz cmpsb %es:(%rdi),%ds:(%rsi)"])
                    , ("repnz scas %es:(%rdi),%al", ["repnz scas %es:(%rdi),%al"])
                    , ("cmp    %dil,%sil", ["cmp    %dil,%sil"])
                    , ("xchg %ax, %ax", ["xchg %ax,%ax"])
                    , ("mov    %al,(%r14,%rbx,1)", ["mov    %al,(%r14,%rbx,1)"])
                    , ("cmpb   $0x0,(%rax,%rbp,1)", ["cmpb   $0x0,(%rax,%rbp,1)"])
                    , ("cmpb   $0x3d,(%rbx,%r13,1)", ["cmpb   $0x3d,(%rbx,%r13,1)"])
                    , ("mov    0x98(%rsp),%rsi", ["mov    0x98(%rsp),%rsi"])
                    , ("movabs $0x7000a38200000000,%rsi", ["movabs $0x7000a38200000000,%rsi"])
                    , ("bt %rcx,%rsi", ["bt %rcx,%rsi"])
                    , ("movzbl 0x8(%rsp),%r9d", ["movzbl 0x8(%rsp),%r9d"])
                    , ("movzwl (%rcx,%rdx,2),%eax", ["movzwl (%rcx,%rdx,2),%eax"])
                    , ("mov    %al,(%r14,%rbx,1)", ["mov    %al,(%r14,%rbx,1)"])
                    , ("xor %r14d,%r14d", ["xor %r14d,%r14d"])
                    , ("mov %rdi,0x601000", ["mov %rdi,0x601000"])
                    , ("mov %rsi,0x601000", ["mov %rsi,0x601000"])
                    , ("mov %rdi,0x501000", ["mov %rdi,0x501000"])
                    , ("mov %edi,0x501000", ["mov %edi,0x501000"])
                    , ("mov %rdi,0x401000", ["mov %rdi,0x401000"])
                    ]

mmxTests :: T.TestTree
mmxTests = T.testGroup "MMXTests" (map mkTest mmxOperandOpcodes)

mmxOperandOpcodes :: [(String, [String])]
mmxOperandOpcodes = [ ("Load a value into an mmx register", ["movq (%eax), %mm2"])
                    , ("mmx xor (reg -> reg)", ["pxor %mm3, %mm0"])
                    , ("mmx xor (mem -> reg)", ["pxor (%rcx), %mm4"])
                    , ("movaps %xmm4,0x90(%rsp)", ["movaps %xmm4,0x90(%rsp)"])
                    ]

sseTests :: T.TestTree
sseTests = T.testGroup "SSETests" (map mkTest sseOperandOpcodes)

sseOperandOpcodes :: [(String, [String])]
sseOperandOpcodes = [ ("sqrt xmmreg (reg -> reg)", ["sqrtps %xmm2, %xmm3"])
                    , ("sqrt xmmreg (mem -> reg)", ["sqrtps (%rax), %xmm4"])
                    , ("sqrt xmmreg (reg -> reg) (extended regs)", ["sqrtps %xmm12, %xmm11"])
                    , ("single reciprocal fp (reg -> reg)", ["rcpps %xmm1, %xmm5"])
                    , ("single reciprocal fp (mem -> reg)", ["rcpps (%eax), %xmm5"])
                    , ("sse xor (reg -> reg)", ["pxor %xmm3, %xmm0"])
                    , ("sse xor (mem -> reg)", ["pxor (%rcx), %xmm4"])
                    , ("shift quad right logical", ["psrldq $0x3, %xmm7"])
                    , ("shift quad right logical (extended)", ["psrldq $0x3, %xmm12"])
                    ]

immediateTests :: T.TestTree
immediateTests =
  T.testGroup "ImmediateOperandOpcodes" (map mkTest immediateOperandOpcodes)

immediateOperandOpcodes :: [(String, [String])]
immediateOperandOpcodes = [ ("push imm8", ["push $3"])
                          , ("push imm16", ["push $15000"])
                          , ("push imm32", ["push $1234567990"])
                          , ("Relative short jump (jmp .+0x28)", ["jmp .+0x28"])
                          , ("Relative near jump (jmp .+0xfff)", ["jmp .+0xfff"])
                          , ("imul   $0xffffffe8,%ebx,%ebx", ["imul   $0xffffffe8,%ebx,%ebx"])
                          ]

mkTest :: (String, [String]) -> T.TestTree
mkTest (name, insns) = T.testCase name $ do
  withAssembledCode Att insns $ \codeBytes -> do
    let disInsns = D.disassembleBuffer codeBytes
--    T.assertEqual "Disassembled instruction count" (length insns) (length disInsns)
    let instances = mapMaybe D.disInstruction disInsns
        assembledInsns = LB.toStrict $ B.toLazyByteString $ mconcat (mapMaybe D.assembleInstruction instances)
    let msg = unlines [ "Assembled bytes:"
                      , prettyHex assembledInsns
                      , "Original bytes:"
                      , prettyHex codeBytes
                      , "Instruction instances:"
                      , show instances
                      ]
    T.assertEqual msg codeBytes assembledInsns
