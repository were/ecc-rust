# Rust Impl of ECC Lang

## Getting Started

````
cargo build --features wasm
#TODO(@were): Is it good to put wasm tools above?
source ../setup.sh # Set up the wasm tools
````

## Try it!

````
# Assuming this is a subrepo of the write-up repo.
./target/debug/ecc ../tests/function/01-helloworld.ecc > 01-helloworld.ll

# A warning will be generated, but it is ok.
emcc 01-helloworld.ll -c

# Expose the main function to js execution.
wasm2wat 01-helloworld.o | sed "s/func \$main/func (export \"main\")/" > 01-helloworld.wat

# Convert WebAssembly text to binaries back.
wat2wasm 01-helloworld.wat # 01-helloworld.wasm is generated

# Run it
# A /dev/null should be redirected to the input.
# O.w., JS's end-of-file (EOF) will not terminate the program.
node ../tests/host.js 01-helloworld.wasm < /dev/null

# You can also use a Ctrl-D to manually pass a EOF to JS's listener.
node ../tests/host.js 01-helloworld.wasm
<Ctrl-D> # Type it after seeing 'Hello world!'
````
