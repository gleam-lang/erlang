//// Working with files on the filesystem.
////
//// The functions included in this module are for high-level concepts such as
//// reading and writing.

/// Reason represents all of the reasons that Erlang surfaces of why a file
/// system operation could fail.
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
}

/// Read the contents of the given file.
///
/// Returns a Result containing the file's contents as a String if the
/// operation was successful, or Reason if the operation failed.
///
/// ## Examples
///
///    > read_file("example.txt")
///    Ok("Hello, World!")
///
///    > read_file("does_not_exist.txt")
///    Error(Enoent)
///
pub external fn read_file(filename: String) -> Result(BitString, Reason) =
  "gleam_erlang_ffi" "read_file"

/// Write the given contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
///    > write_file("file.txt", "Hello, World!")
///    Ok(Nil)
///
///    > write_file("does_not_exist/file.txt", "Hello, World!")
///    Error(Enoent)
///
pub external fn write_file(
  filename: String,
  contents: BitString,
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
