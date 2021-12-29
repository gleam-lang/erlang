# Gleam Erlang 🐙

A library for making use of Erlang specific code!

## Features

- Erlang binary format (de)serialisation.
- Functions for working with Erlang's charlists.
- Reading, writing, and deletion of files.

## Usage

Add this library to your Gleam project

```shell
gleam add gleam_erlang
```

And then use it in your code

```rust
import gleam/io
import gleam/erlang/file

pub fn main() {
  assert Ok(contents) = file.read("pokedex.txt")
  io.println(contents)
}
```
