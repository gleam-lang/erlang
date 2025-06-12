# Gleam Erlang 🐙

Types and functions for programs running on Erlang!

[![Package Version](https://img.shields.io/hexpm/v/gleam_erlang)](https://hex.pm/packages/gleam_erlang)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_erlang/)


```sh
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

This library requires OTP 27.0 or higher.
