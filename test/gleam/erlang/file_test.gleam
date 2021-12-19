import gleam/string
import gleam/erlang/file

pub fn successful_read_test() {
  assert Ok(<<"Hello, World!\n":utf8>>) =
    file.read_file("test/fixtures/success.txt")
}

pub fn unsuccessful_read_test() {
  assert Error(file.Enoent) = file.read_file("test/fixtures/does_not_exist.txt")
}

pub fn successful_write_test() {
  let path = tmp_path("write_test.txt")

  assert Ok(Nil) = file.write_file(path, <<"Hello, World!":utf8>>)
  assert Ok(<<"Hello, World!":utf8>>) = file.read_file(path)

  // Cleanup
  file.delete_file(path)
}

pub fn unsuccessful_write_test() {
  let path = tmp_path("doesnt_exist/write_test.txt")

  assert Error(file.Enoent) = file.write_file(path, <<"Hello, World!":utf8>>)
}

pub fn successful_delete_test() {
  let path = tmp_path("example.txt")
  file.write_file(path, <<>>)

  assert Ok(Nil) = file.delete_file(path)
}

pub fn unsuccessful_delete_test() {
  let path = tmp_path("missing.txt")

  assert Error(file.Enoent) = file.delete_file(path)
}

fn tmp_path(filename: String) {
  string.concat(["test/tmp", filename])
}
