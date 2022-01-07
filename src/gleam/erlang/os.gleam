//// Access to the shell's environment variables

import gleam/map.{Map}

/// Returns the list of all available environment variables as a list of key,
/// tuples.
///
/// ## Examples
///
///    > get_all()
///    map.from_list([
///      #("SHELL", "/bin/bash"),
///      #("PWD", "/home/j3rn"),
///      ...
///    ])
///
pub external fn get_all() -> Map(String, String) =
  "gleam_erlang_ffi" "get_all_env"

/// Returns the value associated with the given environment variable name.
///
/// ## Examples
///
///    > get("SHELL")
///    "/bin/bash"
///
///    > get(name: "PWD")
///    "/home/j3rn"
///
pub external fn get(name: String) -> Result(String, Nil) =
  "gleam_erlang_ffi" "get_env"

/// Associates the given value with the given environment variable name.
///
/// ## Examples
///
///    > set("MYVAR", "MYVALUE")
///    Nil
///    > get("MYVAR")
///    "MYVALUE"
///
///    > set(value: "MYVALUE", name: "MYVAR")
///    Nil
///
pub external fn set(name: String, value: String) -> Nil =
  "gleam_erlang_ffi" "set_env"

/// Removes the environment variable with the given name.
///
/// Returns Nil regardless of whether the variable ever existed.
///
/// ## Examples
///
///    > get("MYVAR")
///    Ok("MYVALUE")
///    > unset("MYVAR")
///    Nil
///    > get("MYVAR")
///    Error(Nil)
///
///    > unset(name: "MYVAR")
///    Nil
///
pub external fn unset(name: String) -> Nil =
  "gleam_erlang_ffi" "unset_env"
