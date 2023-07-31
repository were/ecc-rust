emcc -O0 $1 -c -o ./a.o
wasm2wat a.o | sed "s/func \$main/func (export \"main\")/" > a.wat
wat2wasm a.wat
echo "Binary output to a.wasm"

