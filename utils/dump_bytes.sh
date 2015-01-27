#!/bin/bash

out=`mktemp testfile-XXXXXX`
out_obj=${out%.S}.o

cat <<EOF > $out
    .text
EOF

for i in $@
do
    echo ".byte 0x$i" >> $out
done

gcc -x assembler -c $out
objdump -d $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p'
objdump --disassembler-options=intel-mnemonic -d $out_obj | sed -n 's/^[[:space:]]*0:[[:space:]]*//p'
rm -f $out $out_obj
