# Rust Impl of ECC Lang

The goal of implementing this compiler is to make it `emcc` compatible
so that we can directly compare against LLVM optimized performance.

## Getting Started

````
cargo build --features wasm
source init.sh # Download the wasm binary tools for the 1st time.
source setup.sh # Set up the wasm tools' environment variables.
````

## Try it!

Assuming this is a subrepo of the write-up repo.
You can try this compiler by simply typing:
````
./compile.sh ../tests/function/01-helloworld.ecc
node ./builtins/host.js a.wasm < /dev/null
````


*TL;DR* To understand what happens in this `compile.sh` script,
the file is first compiled into LLVM intermediate representation.

````
./target/debug/ecc ../tests/function/01-helloworld.ecc > a.ll
````

Then we use `emcc` as our LLVM backend to generate wasm binaries.
    
````
emcc a.ll -c
````

Expose the main function to js execution. This is back-and-forth.
The generated binary object is converted to text and then converted back.
````
wasm2wat a.o | sed "s/func \$main/func (export \"main\")/" > a.wat
wat2wasm a.wat # a.wasm is generated
````

You can also use a Ctrl-D to manually pass a EOF to JS's listener, if you do not want a redirection.
````
node ../tests/host.js a.wasm
<Ctrl-D> # Type it after seeing 'Hello world!'
````
