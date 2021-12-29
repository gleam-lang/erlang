import gleam/string
import gleam/erlang/file

pub fn utf8_test() {
  let path = tmp_path("success.txt")
  assert Error(file.Enoent) = file.read(path)

  assert Ok(Nil) = file.write("Hello,\nWorld!", path)
  assert Ok("Hello,\nWorld!") = file.read(path)

  assert Ok(Nil) = file.delete(path)
  assert Error(file.Enoent) = file.read(path)
}

pub fn non_utf8_test() {
  let path = tmp_path("cat.jpeg")
  assert Error(file.Enoent) = file.read_bits(path)

  assert Ok(Nil) = file.write_bits(<<255, 216, 255, 219>>, path)
  assert Error(file.NotUTF8) = file.read(path)
  assert Ok(<<255, 216, 255, 219>>) = file.read_bits(path)

  assert Ok(Nil) = file.delete(path)
  assert Error(file.Enoent) = file.read_bits(path)
}

pub fn non_existent_test() {
  let nonexistent = tmp_path("nonexistent/cat.jpeg")

  assert Error(file.Enoent) = file.read(nonexistent)
  assert Error(file.Enoent) = file.read_bits(nonexistent)
  assert Error(file.Enoent) = file.write("Hello, World!", nonexistent)
  assert Error(file.Enoent) =
    file.write_bits(<<255, 216, 255, 219>>, nonexistent)
}

fn tmp_path(filename: String) {
  string.concat(["test/tmp/", filename])
}
