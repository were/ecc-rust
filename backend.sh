emcc -mllvm --disable-loop-idiom-all -O0 $1 -c -o ./a.o
wasm2wat a.o | sed "s/func \$main/func (export \"main\")/" > a.wat
echo "Assemble text dumped to a.wat"
wat2wasm a.wat
echo "Binary output to a.wasm"

