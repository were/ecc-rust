# Rust Impl of ECC Lang

## Getting Started

````
cargo build --features wasm
source init.sh # Download the wasm binary tools for the 1st time.
source setup.sh # Set up the wasm tools' environment variables.
````

## Try it!

Assuming this is a subrepo of the write-up repo.
````
./target/debug/ecc ../tests/function/01-helloworld.ecc > 01-helloworld.ll
````

Invoke the WebAssembly backend of LLVM. A warning will be generated, but it is ok.
TODO(@were): Add target triple support in [trinity](https://github.com/were/trinity).
````
emcc 01-helloworld.ll -c
````

Expose the main function to js execution. This is back-and-forth.
The generated binary object is converted to text and then converted back.
````
wasm2wat 01-helloworld.o | sed "s/func \$main/func (export \"main\")/" > 01-helloworld.wat
wat2wasm 01-helloworld.wat # 01-helloworld.wasm is generated
````


Run it by invoking a node.js host. A `/dev/null` should be redirected to the input.
Otherwise, JS's end-of-file (EOF) will not terminate the program.
````
node ../tests/host.js 01-helloworld.wasm < /dev/null
````

You can also use a Ctrl-D to manually pass a EOF to JS's listener, if you do not want a redirection.
````
node ../tests/host.js 01-helloworld.wasm
<Ctrl-D> # Type it after seeing 'Hello world!'
````
