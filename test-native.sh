#!/bin/bash

REPO=`git rev-parse --show-toplevel`
FILE=$1

flags=${@:2}

$REPO/./target/debug/ecc $1 --output a.out ${flags} > compile.log 2>&1

echo Compilation log dummped to \"compile.log\"!

if [ $? -ne 0 ]; then
  echo Compilation failed!
  exit 1
fi

grep "IR dumped" compile.log

echo $1 compiled to a.out!

cat $FILE | sed '1,/\[Metadata.stdin\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > input
cat $FILE | sed '1,/\[Metadata.stdout\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > reference

echo "Input data and reference output extracted!"
echo "Run the compiled binaries!"

./a.out < input > output
time ./a.out < input > output

echo "Check the result..."

if diff -q output reference > /dev/null; then
  echo "Correct!"
  rm -f a.wasm input output raw_output reference compile.log
  echo "Everything cleaned up!"
else
  echo "Output wrong!"
  echo "Please check the following files: input, output, reference, compile.log"
  exit 1
fi
