# Rust Impl of ECC Lang

## Getting Started

````
export LLVM_SYS_120_PREFIX=/path/to/clang-llvm-12.0.0/
cargo build
````

This project depends on LLVM 12.0.0. I am using Ubuntu 20.04,
so I downloaded [it](https://github.com/llvm/llvm-project/releases/download/llvmorg-12.0.0/clang+llvm-12.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz)
from [this page](https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.0).

After downloading, just point the `LLVM_SYS_120_PREFIX` variable to
the unzipped folder.

## Try it

````
# Assuming this is a subrepo of the write-up repo.
./target/debug/ecc ../ecc-tests/function/01-helloworld.ecc
````
