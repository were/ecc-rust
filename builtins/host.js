fs = require('fs');

wasm_binary = process.argv[2]

let vbs = process.argv.find(function(x) {
  return x == '--verbose'
});

fd = fs.readFileSync(wasm_binary, undefined)

input_buffer = null

process.stdin.on('data', function (data) {
  if (input_buffer == null) {
    input_buffer = data;
  } else {
    input_buffer = Buffer.concat([input_buffer, data])
  }
})

function __print_int__(x) {
  process.stdout.write(x.toString())
}

memory = null
mem_u8view = null
input_i8view = null

memory_size = 65536
static_size = (1 << 20)
heap_size = static_size

function __print_str__(array) {
  var len =
    (mem_u8view[array + 0]) + 
    (mem_u8view[array + 1] * 256) +
    (mem_u8view[array + 2] * 65536) +
    (mem_u8view[array + 3] * 16777216);
  var offset =
    (mem_u8view[array + 4]) + 
    (mem_u8view[array + 5] * 256) +
    (mem_u8view[array + 6] * 65536) +
    (mem_u8view[array + 7] * 16777216);
  // console.log(mem_u8view[array + 0], mem_u8view[array + 1], mem_u8view[array + 2], mem_u8view[array + 3]);
  // console.log(mem_u8view[array + 4], mem_u8view[array + 5], mem_u8view[array + 6], mem_u8view[array + 7]);
  // console.log('array: ', array, 'len:', len, 'offset:', offset);
  for (i = 0; i < len; ++i) {
    process.stdout.write(String.fromCharCode(mem_u8view[offset + i]))
  }
}

function __malloc__(size) {
  if (heap_size % 8 != 0) {
    heap_size += 8 - heap_size % 8;
  }
  res = heap_size;
  heap_size += size;
  console.log('malloc', res, 'size', size);
  return res
}

input_ptr = 0;
function nextInt() {
  while (input_ptr < input_buffer.length && (input_buffer[input_ptr] == 32 || // space
                                          input_buffer[input_ptr] == 10 || // \n
                                          input_buffer[input_ptr] == 13 || // \r
                                          input_buffer[input_ptr] == 9)) { // \t
    input_ptr++;
  }
  res = 0;
  while (input_ptr < input_buffer.length && (input_buffer[input_ptr] >= 48 && input_buffer[input_ptr] <= 57)) {
    res = res * 10 + (input_buffer[input_ptr] - 48);
    input_ptr++;
  }
  return res;
}

__linear_memory = new WebAssembly.Memory({initial: memory_size})
__stack_pointer = new WebAssembly.Global({value: "i32", mutable: true}, memory_size)

imports = {
  env: {
    __print_int__: __print_int__,
    __print_str__: __print_str__,
    malloc: __malloc__,
    __linear_memory: __linear_memory,
    __stack_pointer: __stack_pointer,
    nextInt: nextInt,
  }
}

WebAssembly.instantiate(fd, imports).then(function (result) {
  mem_u8view = new Uint8Array(__linear_memory.buffer)
  if (vbs) {
    console.time('Exec time');
  }
  var res = result.instance.exports.main(0, 0)
  if (vbs) {
    console.timeEnd('Exec time');
    console.log('Exit code:', res);
  }
})

