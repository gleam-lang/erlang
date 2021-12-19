//// Working with files on the filesystem.
////
//// The functions included in this module are for high-level concepts such as
//// reading and writing.

/// There are various reasons why a file system operation could fail. Reason
/// represents all of the reasons that Erlang surfaces. These are separated into
/// two categories:
///
/// ## POSIX Errors
/// - `Eacces` :: Permission denied.
/// - `Eagain` :: Resource temporarily unavailable.
/// - `Ebadf` :: Bad file number.
/// - `Ebadmsg` :: Bad message.
/// - `Ebusy` :: File busy.
/// - `Edeadlk` :: Resource deadlock avoided.
/// - `Edeadlock` :: On most architectures, same as `Edeadlk`. On some
///  architectures, it means "File locking deadlock error."
/// - `Edquot` :: Disk quota exceeded.
/// - `Eexist` :: File already exists.
/// - `Efault` :: Bad address in system call argument.
/// - `Efbig` :: File too large.
/// - `Eftype` :: Inappropriate file type or format. Usually caused by trying to
///  set the "sticky bit" on a regular file (not a directory).
/// - `Eintr` :: Interrupted system call.
/// - `Einval` :: Invalid argument.
/// - `Eio` :: I/O error.
/// - `Eisdir` :: Illegal operation on a directory.
/// - `Eloop` :: Too many levels of symbolic links.
/// - `Emfile` :: Too many open files.
/// - `Emlink` :: Too many links.
/// - `Emultihop` :: Multihop attempted.
/// - `Enametoolong` :: Filename too long.
/// - `Enfile` :: File table overflow
/// - `Enobufs` :: No buffer space available.
/// - `Enodev` :: No such device.
/// - `Enolck` :: No locks available.
/// - `Enolink` :: Link has been severed.
/// - `Enoent` :: No such file or directory.
/// - `Enomem` :: Not enough memory.
/// - `Enospc` :: No space left on device.
/// - `Enosr` :: No STREAM resources.
/// - `Enostr` :: Not a STREAM.
/// - `Enosys` :: Function not implemented.
/// - `Enotblk` :: Block device required.
/// - `Enotdir` :: Not a directory.
/// - `Enotsup` :: Operation not supported.
/// - `Enxio` :: No such device or address.
/// - `Eopnotsupp` :: Operation not supported on socket.
/// - `Eoverflow` :: Value too large to be stored in data type.
/// - `Eperm` :: Not owner.
/// - `Epipe` :: Broken pipe.
/// - `Erange` :: Result too large.
/// - `Erofs` :: Read-only file system.
/// - `Espipe` :: Invalid seek.
/// - `Esrch` :: No such process.
/// - `Estale` :: Stale remote file handle.
/// - `Etxtbsy` :: Text file busy.
/// - `Exdev` :: Cross-domain link.
///
/// ## Erlang Errors
/// - `Badarg` :: Bad argument; usually the argument is of an invalid type. Due
///  to Gleam's strong typing, this error should not be encountered when the
///  functions of this module are called from Gleam code.
/// - `Terminated` :: File server process is terminated.
/// - `SystemLimit` :: A system limit was hit, probably not enough ports.
pub type Reason {
  Eacces
  Eagain
  Ebadf
  Ebadmsg
  Ebusy
  Edeadlk
  Edeadlock
  Edquot
  Eexist
  Efault
  Efbig
  Eftype
  Eintr
  Einval
  Eio
  Eisdir
  Eloop
  Emfile
  Emlink
  Emultihop
  Enametoolong
  Enfile
  Enobufs
  Enodev
  Enolck
  Enolink
  Enoent
  Enomem
  Enospc
  Enosr
  Enostr
  Enosys
  Enotblk
  Enotdir
  Enotsup
  Enxio
  Eopnotsupp
  Eoverflow
  Eperm
  Epipe
  Erange
  Erofs
  Espipe
  Esrch
  Estale
  Etxtbsy
  Exdev
  Bardarg
  Terminated
  SystemLimit
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
pub external fn read_file(filename: String) -> Result(String, Reason) =
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
  contents: String,
) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "write_file"

/// Delete the given file.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
///    > delete_file("file.txt")
///    Ok(Nil)
///
///    > delete_file("does_not_exist.txt")
///    Error(Enoent)
///
pub external fn delete_file(filename: String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "delete_file"
