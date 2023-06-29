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
./target/debug/ecc ../tests/function/01-helloworld.ecc
node ./builtins/host.js a.wasm < /dev/null
````

You can also use a Ctrl-D to manually pass a EOF to JS's listener, if you do not want a null-input redirection.
````
node ../tests/host.js a.wasm
<Ctrl-D> # Type it after seeing 'Hello world!'
````
