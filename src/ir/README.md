# Trinity: Building LLVM-IR in Rust without Binding

**Why yet another LLVM-IR infrastructure in Rust?**

Correct me if I am wrong. I had a hard time to find a crate that exactly fits my requirement.
I want to write a LLVM and Rust based compiler tutorial, but:

0. [llvm-sys](https://docs.rs/llvm-sys/160.0.2/llvm_sys/) is a C-Rust
   bind for the C interfaces exposed by LLVM,
   which suffers from infinte `unsafe` annotations.
1. [inkwell](https://github.com/TheDan64/inkwell) wraps all those C-interfaces
   with Rust native data structures to get rid of `unsafe`s, but
     - I had a hard time to convert back-and-forth across their `{Any/Basic}{Value/Type}`s.
     - It makes me realize that both `llvm-sys` just exposes a subset of LLVM data structures,
       and `inkwell` is a subset of `llvm-sys`. I need a full control to the struct fields.
2. [llvm-ir](https://docs.rs/llvm-ir/latest/llvm_ir/) can only do
   read and analysis, which is not a LLVM emitter.


Admittedly, I learned many from `inkwell`'s API design, and I hope this will be easier to use.
I will prove the usability of this set of APIs, through my compiler tutorial.

**Where does the name Trinity come from?**

I just merge the `Context`, `Module`, and `IRBuilder` together
to both fit Rust's convention, and deal with the redundancy of LLVM IR data structures.
