#!/bin/bash

REPO=`git rev-parse --show-toplevel`
FILE=$1

$REPO/./target/debug/ecc $1 --backend myown --output a.wasm --opt 2 > compile.log 2>&1

if [ $? -ne 0 ]; then
  echo Compilation failed!
  exit 1
fi

echo $1 compiled to a.wasm!
echo Compilation log dummped to \"compile.log\"!

cat $FILE | sed '1,/\[Metadata.stdin\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > input
cat $FILE | sed '1,/\[Metadata.stdout\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > reference

echo "Input data and reference output extracted!"
echo "Run the compiled binaries!"

node $REPO/./builtins/host.js a.wasm < input > output

echo "Check the result..."

if diff -q output reference > /dev/null; then
  echo "Correct!"
  rm -f a.wasm input output reference compile.log
  echo "Everything cleaned up!"
else
  echo "Output wrong!"
fi
