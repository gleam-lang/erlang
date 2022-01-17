import gleam/erlang/os

pub fn command_test() {
  // Not sure Windows has an 'echo' command; might be *nix only.
  assert "Hello, World!\n" = os.command("echo \"Hello, World!\"")
}

pub fn kernel_test() {
  // Ok, I _really_ doubt Windows has a `uname`.
  case os.cmd("uname -s") {
    "Linux" -> os.Linux
    "Darwin" -> os.Darwin
    other -> os.Other(other)
  }
}
