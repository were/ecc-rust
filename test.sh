#!/bin/bash

# Usage: ./test.sh [file-name] [target] [--timeit/--exec] [flags]
REPO=`git rev-parse --show-toplevel`
FILE=$1
TARGET=$2

# Parse the arguments
FLAGS=${@:4}

# Pass the flags to the compiler.
$REPO/./target/debug/ecc $1 --target ${TARGET} --output a.out ${FLAGS} > compile.log 2>&1

echo "Compiling $1 to ${TARGET}..."
grep "IR dumped" compile.log

if [ $? -ne 0 ]; then
  echo Compilation failed!
  exit 1
fi

echo $1 compiled to a.out!
echo Compilation log dummped to \"compile.log\"!

# Extract the input data and reference output.
cat $FILE | sed '1,/\[Metadata.stdin\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > input
cat $FILE | sed '1,/\[Metadata.stdout\]/d' | sed '/\[Metadata.end\]/,$d' | awk '{ print substr($0, 4) }' > reference

echo "Input data and reference output extracted!"
echo "Run the compiled binaries!"

TIMEIT=`which time`
if [ $TARGET == "wasm" ]; then
  if [ $3 == "--exec" ]; then
    node --stack-size=1048576 $REPO/./builtins/host.js a.out --verbose < input > raw_output
    lines=$(( $(cat raw_output | wc -l) - 2 ))
    if [ $lines != "0" ]; then
      head -n $lines raw_output > output
    else
      touch output
    fi
  else
    sum=0
    for i in {1..10}; do
      node --stack-size=1048576 $REPO/./builtins/host.js a.out --verbose < input > raw_output
      ms=`grep "Exec time: " raw_output | awk '{ print $3 }' | sed 's/ms//'`
      sum=`echo $sum + $ms | bc`
    done
    $TIMEIT -l -h -o a.log node --stack-size=1048576 $REPO/./builtins/host.js a.out --verbose < input > raw_output
    insts=`grep "instructions retired" a.log | awk '{ print $1 }'`
    printf "Instructions retired: %d\n" `echo $insts - 413945502 | bc`
    rm a.log
    sum=`echo "$sum / 10" | bc -l`
    printf "Average exec time: %.2f ms\n" $sum
  fi
else
  if [ $3 == "--exec" ]; then
    ./a.out < input > output
  else
    $TIMEIT -l -h -o a.log ./a.out < input > output
    insts=`grep "instructions retired" a.log | awk '{ print $1 }'`
    echo "Instructions retired: $insts"
    rm a.log
  fi
fi

if [ $3 == "--exec" ]; then
  echo "Checking the result..."
  if diff -q output reference > /dev/null; then
    echo "Correct!"
    rm -f a.out input output raw_output reference compile.log
    echo "Everything cleaned up!"
  else
    echo "Output wrong!"
    echo "Please check the following files: input, output, reference, compile.log"
    exit 1
  fi
else
  rm -f a.out input output raw_output reference compile.log
fi
