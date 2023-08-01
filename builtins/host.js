fs = require('fs');

wasm_binary = process.argv[2]

let vbs = process.argv.find(function(x) {
  return x == '--verbose'
});

fd = fs.readFileSync(wasm_binary, undefined)

input_buffer = []

process.stdin.on('data', function (data) {
  input_buffer = new Int8Array(data);
  input_buffer.length = data.length;
})

function __print_int__(x) {
  process.stdout.write(x.toString())
}

memory = null
mem_i8view = null
input_i8view = null

memory_size = (1 << 20)
static_size = (1 << 10)
heap_size = static_size

function __print_str__(offset, len) {
  for (i = 0; i < len; ++i) {
    process.stdout.write(String.fromCharCode(mem_i8view[offset + i]))
  }
}

function __malloc__(size) {
  res = heap_size
  heap_size += size
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

__linear_memory = new WebAssembly.Memory({initial: 65536})
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
  mem_i8view = new Int8Array(__linear_memory.buffer)
  if (vbs) {
    console.time('Exec time');
  }
  var res = result.instance.exports.main(0, 0)
  if (vbs) {
    console.timeEnd('Exec time');
    console.log('Exit code:', res);
  }
})

