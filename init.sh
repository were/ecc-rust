#!/bin/env zsh

source_dir=`pwd`
script_dir=`dirname ${0}`
cd $script_dir
script_dir=`pwd`

mkdir -p $script_dir/bin-utils

# Download WebAssembly binary tools
cd $script_dir/bin-utils
if [ ! -d wabt ] ; then
  git clone --recursive https://github.com/WebAssembly/wabt
  cd wabt
else
  cd wabt
  git pull
  git submodule update --init --recursive
fi
cmake -S . -B build
make -C build -j

# Download LLVM-WebAssembly backend
cd $script_dir/bin-utils
if [ ! -d emsdk ] ; then
  git clone --recursive https://github.com/emscripten-core/emsdk
  cd emsdk
else
  cd emsdk
  git pull
  git submodule update --init --recursive
fi
./emsdk install latest
./emsdk activate latest
cd $source_dir

