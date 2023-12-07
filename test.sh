#!/bin/bash

REPO=`git rev-parse --show-toplevel`
FILE=$1

flags=${@:2}

$REPO/./target/debug/ecc $1 --output a.wasm ${flags} > compile.log 2>&1

grep "IR dumped" compile.log

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

node --stack-size=1048576 $REPO/./builtins/host.js a.wasm --verbose < input > raw_output

tail -n 2 raw_output

lines=$(( $(cat raw_output | wc -l) - 2 ))
if [ $lines != "0" ]; then
  head -n $lines raw_output > output
else
  touch output
fi

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
