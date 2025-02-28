# Gleam Erlang 🐙

A library for making use of Erlang specific code!

```shell
gleam add gleam_erlang@1
```
```gleam
import gleam/io
import gleam/erlang/process

pub fn main() {
  process.spawn(fn() { 
    io.println("Hello from another process running concurrently!")
  })
}
```

Documentation can be found at <https://hexdocs.pm/gleam_erlang/>.

This library requires OTP 23.0 or higher.
