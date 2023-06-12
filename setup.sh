#!/usr/bin/env zsh

source_dir=`pwd`
script_dir=`dirname ${0}`

# Set up environment variables for WebAssembly binary tools.
cd $script_dir/backend/wabt
export PATH=`pwd`/build:$PATH
cd $source_dir

# Set up environment variables for LLVM-WebAssembly backend.
cd $script_dir/backend/emsdk
source ./emsdk_env.sh
cd $source_dir
