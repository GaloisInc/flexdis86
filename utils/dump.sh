#!/bin/bash

# Assemble the ASM instruction given by the first argument (Intel
# syntax) and then dump the flexdix86 representation of that
# instruction to stdout. For example
#
#     ./dump.sh "add %eax, %eax"
#
# This script assumes that the flexdis86 'DumpInstr' binary is on
# PATH. So, you probably want to do
#
#     cabal new-exec ./dump.sh <asm>
#
# or
#
#     stack exec ./dump.sh <asm>
#
# to make 'DumpInstr' available.

set -e

if ! which DumpInstr &>/dev/null; then
  echo "No DumpInstr binary on PATH. See below:" >&2
  echo "" >&2
  cat $0 >&2
  exit 1
fi

mkdir -p tmp
out=`mktemp tmp/testfile-XXXXXX.S`
out_obj=${out%.S}.o

cat <<EOF > $out
    .intel_syntax noprefix
    .text
    $@
EOF

gcc -x assembler -c $out -o $out_obj
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p'
objdump --disassembler-options=intel-mnemonic -dw $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p' | cut -f1 | xargs DumpInstr
echo $out $out_obj
