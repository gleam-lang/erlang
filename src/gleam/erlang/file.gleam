//// Working with files on the filesystem.
////
//// The functions included in this module are for high-level concepts such as
//// reading and writing.

import gleam/bit_string
import gleam/result

/// Reason represents all of the reasons that Erlang surfaces of why a file
/// system operation could fail. Most of these reasons are POSIX errors, which
/// come from the operating system and start with `E`. Others have been added to
/// represent other issues that may arise.
pub type Reason {
  /// Permission denied.
  Eacces
  /// Resource temporarily unavailable.
  Eagain
  /// Bad file number
  Ebadf
  /// Bad message.
  Ebadmsg
  /// File busy.
  Ebusy
  /// Resource deadlock avoided.
  Edeadlk
  /// On most architectures, same as `Edeadlk`. On some architectures, it
  /// means "File locking deadlock error."
  Edeadlock
  /// Disk quota exceeded.
  Edquot
  /// File already exists.
  Eexist
  /// Bad address in system call argument.
  Efault
  /// File too large.
  Efbig
  /// Inappropriate file type or format. Usually caused by trying to set the
  /// "sticky bit" on a regular file (not a directory).
  Eftype
  /// Interrupted system call.
  Eintr
  /// Invalid argument.
  Einval
  /// I/O error.
  Eio
  /// Illegal operation on a directory.
  Eisdir
  /// Too many levels of symbolic links.
  Eloop
  /// Too many open files.
  Emfile
  /// Too many links.
  Emlink
  /// Multihop attempted.
  Emultihop
  /// Filename too long
  Enametoolong
  /// File table overflow
  Enfile
  /// No buffer space available.
  Enobufs
  /// No such device.
  Enodev
  /// No locks available.
  Enolck
  /// Link has been severed.
  Enolink
  /// No such file or directory.
  Enoent
  /// Not enough memory.
  Enomem
  /// No space left on device.
  Enospc
  /// No STREAM resources.
  Enosr
  /// Not a STREAM.
  Enostr
  /// Function not implemented.
  Enosys
  /// Block device required.
  Enotblk
  /// Not a directory.
  Enotdir
  /// Operation not supported.
  Enotsup
  /// No such device or address.
  Enxio
  /// Operation not supported on socket.
  Eopnotsupp
  /// Value too large to be stored in data type.
  Eoverflow
  /// Not owner.
  Eperm
  /// Broken pipe.
  Epipe
  /// Result too large.
  Erange
  /// Read-only file system.
  Erofs
  /// Invalid seek.
  Espipe
  /// No such process.
  Esrch
  /// Stale remote file handle.
  Estale
  /// Text file busy.
  Etxtbsy
  /// Cross-domain link.
  Exdev
  /// File was requested to be read as UTF-8, but is not UTF-8 encoded.
  NotUtf8
}

/// Returns true if path refers to a directory, otherwise false.
///
/// ## Examples
///
///    > is_directory("/tmp")
///    True
///
///    > is_directory("resume.pdf")
///    False
pub external fn is_directory(path: String) -> Bool =
  "filelib" "is_dir"

/// Returns true if path refers to a file, otherwise false.
///
/// ## Examples
///
///    > is_file("resume.pdf")
///    True
///
///    > is_file("/tmp")
///    True
///
///    > is_file("/does_not_exist")
///    False
pub external fn is_file(path: String) -> Bool =
  "filelib" "is_file"

/// Tries to create a directory. Missing parent directories are not created.
///
/// Returns a Result of nil if the directory is created or Reason if the
/// operation failed.
///
/// ## Examples
///
///    > make_directory("/tmp/foo")
///    Ok(Nil)
///
///    > make_directory("relative_directory")
///    Ok(Nil)
///
///    > make_directory("/tmp/missing_intermediate_directory/foo")
///    Error(Enoent)
pub external fn make_directory(path: String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "make_directory"

/// Lists all files in a directory, except files with
/// [raw filenames](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#notes-about-raw-filenames).
///
/// Returns a Result containing the list of filenames in the directory, or Reason
/// if the operation failed.
///
/// ## Examples
///
///    > list_directory("/tmp")
///    Ok(["FB01293B-8597-4359-80D5-130140A0C0DE","AlTest2.out"])
///
///    > list_directory("resume.docx")
///    Error(Enotdir)
pub external fn list_directory(path: String) -> Result(List(String), Reason) =
  "gleam_erlang_ffi" "list_directory"

/// Deletes a directory.
///
/// The directory must be empty before it can be deleted. Returns a nil Success
/// or Reason if the operation failed.
///
/// ## Examples
///
///    > delete_directory("foo")
///    Ok(Nil)
///
///    > delete_directory("does_not_exist/")
///    Error(Enoent)
pub external fn delete_directory(path: String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "delete_directory"

/// Deletes a file or directory recursively.
///
/// Returns a nil Success or Reason if the operation failed.
///
/// ## Examples
///
///    > recursive_delete("foo")
///    Ok(Nil)
///
///    > recursive_delete("/bar")
///    Ok(Nil)
///
///    > recursive_delete("does_not_exist/")
///    Error(Enoent)
pub external fn recursive_delete(path: String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "recursive_delete"

/// Read the contents of the given file as a String
///
/// Assumes the file is UTF-8 encoded. Returns a Result containing the file's
/// contents as a String if the operation was successful, or Reason if the file
/// operation failed. If the file is not UTF-8 encoded, the `NotUTF8` variant
/// will be returned.
///
/// ## Examples
///
///    > read("example.txt")
///    Ok("Hello, World!")
///
///    > read(from: "example.txt")
///    Ok("Hello, World!")
///
///    > read("does_not_exist.txt")
///    Error(Enoent)
///
///    > read("cat.gif")
///    Error(NotUTF8)
///
pub fn read(from path: String) -> Result(String, Reason) {
  path
  |> do_read_bits()
  |> result.then(fn(content) {
    case bit_string.to_string(content) {
      Ok(string) -> Ok(string)
      Error(Nil) -> Error(NotUtf8)
    }
  })
}

/// Read the contents of the given file as a BitString
///
/// Returns a Result containing the file's contents as a BitString if the
/// operation was successful, or Reason if the operation failed.
///
/// ## Examples
///
///    > read_bits("example.txt")
///    Ok(<<"Hello, World!">>)
///
///    > read_bits(from: "cat.gif")
///    Ok(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>)
///
///    > read_bits("does_not_exist.txt")
///    Error(Enoent)
///
pub fn read_bits(from path: String) -> Result(BitString, Reason) {
  do_read_bits(path)
}

external fn do_read_bits(path) -> Result(BitString, Reason) =
  "gleam_erlang_ffi" "read_file"

/// Write the given String contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
///    > write("Hello, World!", "file.txt")
///    Ok(Nil)
///
///    > write(to: "file.txt", contents: "Hello, World!")
///    Ok(Nil)
///
///    > write("Hello, World!", "does_not_exist/file.txt")
///    Error(Enoent)
///
pub fn write(contents contents: String, to path: String) -> Result(Nil, Reason) {
  contents
  |> bit_string.from_string
  |> do_write_bits(path)
}

/// Write the given BitString contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
///    > write_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "cat.gif")
///    Ok(Nil)
///
///    > write_bits(to: "cat.gif", contents: <<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>)
///    Ok(Nil)
///
///    > write_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "does_not_exist/cat.gif")
///    Error(Enoent)
///
pub fn write_bits(
  contents contents: BitString,
  to path: String,
) -> Result(Nil, Reason) {
  do_write_bits(contents, path)
}

external fn do_write_bits(
  contents: BitString,
  path: String,
) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "write_file"

/// Delete the given file.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
///    > delete("file.txt")
///    Ok(Nil)
///
///    > delete("does_not_exist.txt")
///    Error(Enoent)
///
pub external fn delete(String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "delete_file"
