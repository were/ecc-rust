fs = require('fs');

wasm_binary = process.argv[2]

fd = fs.readFileSync(wasm_binary, undefined)

input_buffer = []

process.stdin.on('data', function (data) {
  input_buffer.push(data)
})

function __print_int__(x) {
  process.stdout.write(x.toString())
}

memory = null
i8view = null

memory_size = (1 << 20)
static_size = (1 << 10)
heap_size = static_size

function __print_str__(offset, len) {
  for (i = 0; i < len; ++i) {
    process.stdout.write(String.fromCharCode(i8view[offset + i]))
  }
}

function __malloc__(size) {
  res = heap_size
  heap_size += size
  return res
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
  }
}

WebAssembly.instantiate(fd, imports).then(function (result) {

  // memory = result.instance.exports.memory
  i8view = new Int8Array(__linear_memory.buffer)

  result.instance.exports.main(0, 0)
})

