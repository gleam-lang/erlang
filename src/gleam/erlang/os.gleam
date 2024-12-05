/// Represents operating system kernels
pub type OsFamily {
  // The family which includes modern versions of the Windows operating system.
  WindowsNt
  // The family of operating systems based on the open source Linux kernel.
  Linux
  // The family of Apple operating systems such as macOS and iOS.
  Darwin
  // The family of operating systems based on the FreeBSD kernel.
  FreeBsd
  // An operating system kernel other than Linux, Darwin, FreeBSD, or NT.
  Other(String)
}

/// Returns the kernel of the host operating system.
///
/// Unknown kernels are reported as `Other(String)`; e.g. `Other("sunos")`.
///
/// ## Examples
/// ```gleam
/// family()
/// // -> Linux
/// ```
/// 
/// ```gleam
/// family()
/// // -> Darwin
/// ```
/// 
/// ```gleam
/// family()
/// // -> Other("sunos")
/// ```
///
@external(erlang, "gleam_erlang_ffi", "os_family")
pub fn family() -> OsFamily
