import gleam/erlang/os

pub fn command_test() {
  // Not sure Windows has an 'echo' command; might be *nix only.
  assert "Hello, World!\n" = os.command("echo \"Hello, World!\"")
}

pub fn kernel_test() {
  // I doubt Windows has a `uname`, reconsider testing approach
  case os.command("uname -s") {
    "Linux\n" -> {
      assert os.Linux = os.kernel()
    }
    "Darwin\n" -> {
      assert os.Darwin = os.kernel()
    }
    _ -> {
      assert os.Other(_) = os.kernel()
    }
  }
}
