import gleam/string
import gleam/erlang/file

const tmp_directory = "test/tmp/"

pub fn utf8_test() {
  make_tmp_directory()
  let path = tmp_path("success.txt")
  let assert Error(file.Enoent) = file.read(path)

  let assert Ok(Nil) = file.write("Hello,\nWorld!", path)
  let assert Ok("Hello,\nWorld!") = file.read(path)

  let assert Ok(Nil) = file.delete(path)
  let assert Error(file.Enoent) = file.read(path)
  delete_tmp_directory()
}

pub fn non_utf8_test() {
  make_tmp_directory()
  let path = tmp_path("cat.jpeg")
  let assert Error(file.Enoent) = file.read_bits(path)

  let assert Ok(Nil) = file.write_bits(<<255, 216, 255, 219>>, path)
  let assert Error(file.NotUtf8) = file.read(path)
  let assert Ok(<<255, 216, 255, 219>>) = file.read_bits(path)

  let assert Ok(Nil) = file.delete(path)
  let assert Error(file.Enoent) = file.read_bits(path)
  delete_tmp_directory()
}

pub fn non_existent_test() {
  make_tmp_directory()
  let nonexistent = tmp_path("nonexistent/cat.jpeg")

  let assert Error(file.Enoent) = file.read(nonexistent)
  let assert Error(file.Enoent) = file.read_bits(nonexistent)
  let assert Error(file.Enoent) = file.write("Hello, World!", nonexistent)
  let assert Error(file.Enoent) =
    file.write_bits(<<255, 216, 255, 219>>, nonexistent)
  delete_tmp_directory()
}

pub fn label_test() {
  make_tmp_directory()
  let path = tmp_path("success.txt")
  let assert Error(file.Enoent) = file.read(from: path)

  let assert Ok(Nil) = file.write(to: path, contents: "Hello,\nWorld!")
  let assert Ok("Hello,\nWorld!") = file.read(from: path)

  let assert Ok(Nil) =
    file.write_bits(to: path, contents: <<255, 216, 255, 219>>)
  let assert Ok(<<255, 216, 255, 219>>) = file.read_bits(from: path)

  let assert Ok(Nil) = file.delete(path)
  let assert Error(file.Enoent) = file.read(from: path)
  delete_tmp_directory()
}

pub fn dir_test() {
  make_tmp_directory()
  let path = tmp_path("missing_dir/foo")
  let assert Error(file.Enoent) = file.make_directory(path: path)
  let assert False = file.is_directory(path: path)
  let assert False = file.is_file(path: path)

  let path = tmp_path("bar")
  let assert Ok(Nil) = file.make_directory(path: path)
  let assert True = file.is_directory(path: path)
  let assert True = file.is_file(path: path)

  let nested_path = tmp_path("bar/baz")
  let assert Ok(Nil) = file.make_directory(path: nested_path)
  let assert Ok(Nil) = file.recursive_delete(path: path)
  let assert Error(file.Enoent) = file.delete_directory(path: path)

  delete_tmp_directory()
}

fn tmp_path(filename: String) {
  string.concat([tmp_directory, filename])
}

fn make_tmp_directory() {
  delete_tmp_directory()
  let assert Ok(Nil) = file.make_directory(tmp_directory)
  Nil
}

fn delete_tmp_directory() {
  let assert Ok(Nil) = case file.recursive_delete(tmp_directory) {
    Error(file.Enoent) -> Ok(Nil)
    other -> other
  }
  Nil
}

pub fn append_test() {
  make_tmp_directory()
  let path = tmp_path("success.txt")
  let assert Error(file.Enoent) = file.read(path)

  let assert Ok(Nil) = file.append("one", path)
  let assert Ok("one") = file.read(path)

  let assert Ok(Nil) = file.append("two", path)
  let assert Ok("onetwo") = file.read(path)

  let assert Ok(Nil) = file.append("three", path)
  let assert Ok("onetwothree") = file.read(path)

  let assert Ok(Nil) = file.delete(path)
  let assert Error(file.Enoent) = file.read(path)
  delete_tmp_directory()
}

pub fn append_bits_test() {
  make_tmp_directory()
  let path = tmp_path("cat.jpeg")
  let assert Error(file.Enoent) = file.read_bits(path)

  let assert Ok(Nil) = file.append_bits(<<1>>, path)
  let assert Ok(<<1>>) = file.read_bits(path)

  let assert Ok(Nil) = file.append_bits(<<2>>, path)
  let assert Ok(<<1, 2>>) = file.read_bits(path)

  let assert Ok(Nil) = file.append_bits(<<3>>, path)
  let assert Ok(<<1, 2, 3>>) = file.read_bits(path)

  let assert Ok(Nil) = file.delete(path)
  let assert Error(file.Enoent) = file.read_bits(path)
  delete_tmp_directory()
}
