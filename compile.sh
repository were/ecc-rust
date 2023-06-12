fname=${1%.*}
dirname=`dirname $1`
curdir=`pwd`
script_dir=`dirname $0`
cd $script_dir
script_dir=`pwd`
cd $curdir

$script_dir/target/debug/ecc $1 > a.ll
emcc a.ll -c

wasm2wat a.o | sed "s/func \$main/func (export \"main\")/" > a.wat
wat2wasm a.wat

