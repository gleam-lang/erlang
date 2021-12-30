import gleam/string
import gleam/erlang/file

const tmp_directory = "test/tmp/"

pub fn utf8_test() {
  make_tmp_directory()
  let path = tmp_path("success.txt")
  assert Error(file.Enoent) = file.read(path)

  assert Ok(Nil) = file.write("Hello,\nWorld!", path)
  assert Ok("Hello,\nWorld!") = file.read(path)

  assert Ok(Nil) = file.delete(path)
  assert Error(file.Enoent) = file.read(path)
  delete_tmp_directory()
}

pub fn non_utf8_test() {
  make_tmp_directory()
  let path = tmp_path("cat.jpeg")
  assert Error(file.Enoent) = file.read_bits(path)

  assert Ok(Nil) = file.write_bits(<<255, 216, 255, 219>>, path)
  assert Error(file.NotUTF8) = file.read(path)
  assert Ok(<<255, 216, 255, 219>>) = file.read_bits(path)

  assert Ok(Nil) = file.delete(path)
  assert Error(file.Enoent) = file.read_bits(path)
  delete_tmp_directory()
}

pub fn non_existent_test() {
  make_tmp_directory()
  let nonexistent = tmp_path("nonexistent/cat.jpeg")

  assert Error(file.Enoent) = file.read(nonexistent)
  assert Error(file.Enoent) = file.read_bits(nonexistent)
  assert Error(file.Enoent) = file.write("Hello, World!", nonexistent)
  assert Error(file.Enoent) =
    file.write_bits(<<255, 216, 255, 219>>, nonexistent)
  delete_tmp_directory()
}

pub fn label_test() {
  make_tmp_directory()
  let path = tmp_path("success.txt")
  assert Error(file.Enoent) = file.read(from: path)

  assert Ok(Nil) = file.write(to: path, contents: "Hello,\nWorld!")
  assert Ok("Hello,\nWorld!") = file.read(from: path)

  assert Ok(Nil) = file.write_bits(to: path, contents: <<255, 216, 255, 219>>)
  assert Ok(<<255, 216, 255, 219>>) = file.read_bits(from: path)

  assert Ok(Nil) = file.delete(path)
  assert Error(file.Enoent) = file.read(from: path)
  delete_tmp_directory()
}

pub fn dir_test() {
  make_tmp_directory()
  let path = tmp_path("missing_dir/foo")
  assert Error(file.Enoent) = file.make_directory(path: path)
  assert False = file.is_directory(path: path)
  assert False = file.is_file(path: path)

  let path = tmp_path("bar")
  assert Ok(Nil) = file.make_directory(path: path)
  assert True = file.is_directory(path: path)
  assert True = file.is_file(path: path)

  let nested_path = tmp_path("bar/baz")
  assert Ok(Nil) = file.make_directory(path: nested_path)
  assert Ok(Nil) = file.recursive_delete(path: path)
  assert Error(file.Enoent) = file.delete_directory(path: path)

  delete_tmp_directory()
}

fn tmp_path(filename: String) {
  string.concat([tmp_directory, filename])
}

fn make_tmp_directory() {
  delete_tmp_directory()
  assert Ok(Nil) = file.make_directory(tmp_directory)
}

fn delete_tmp_directory() {
  assert Ok(Nil) = case file.recursive_delete(tmp_directory) {
    Error(file.Enoent) -> Ok(Nil)
    other -> other
  }
}
