import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/list
import gleam/map
import gleam/result
import gleam/string
import gleam/erlang/charlist.{Charlist}

external fn erl_format(String, List(a)) -> Charlist =
  "io_lib" "format"

/// Return a string representation of any term
pub fn format(term: any) -> String {
  charlist.to_string(erl_format("~p", [term]))
}

pub external fn term_to_binary(a) -> BitString =
  "erlang" "term_to_binary"

type Safe {
  Safe
}

external fn erl_binary_to_term(BitString, List(Safe)) -> Dynamic =
  "erlang" "binary_to_term"

pub fn binary_to_term(binary: BitString) -> Result(Dynamic, Nil) {
  case rescue(fn() { erl_binary_to_term(binary, [Safe]) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

pub fn unsafe_binary_to_term(binary: BitString) -> Result(Dynamic, Nil) {
  case rescue(fn() { erl_binary_to_term(binary, []) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

/// Error value returned by `get_line` function
///
pub type GetLineError {
  Eof
  NoData
}

/// Reads a line from standard input with the given prompt.
///
/// # Example
///
///    > get_line("Language: ")
///    // -> Language: <- gleam
///    Ok("gleam\n")
///
pub external fn get_line(prompt: String) -> Result(String, GetLineError) =
  "gleam_erlang_ffi" "get_line"

pub type TimeUnit {
  Second
  Millisecond
  Microsecond
  Nanosecond
}

/// Returns the current OS system time.
///
/// https://erlang.org/doc/apps/erts/time_correction.html#OS_System_Time
pub external fn system_time(TimeUnit) -> Int =
  "os" "system_time"

/// Returns the current OS system time as a tuple of Ints
///
/// http://erlang.org/doc/man/os.html#timestamp-0
pub external fn erlang_timestamp() -> #(Int, Int, Int) =
  "os" "timestamp"

/// Gleam doesn't offer any way to raise exceptions, but they may still occur
/// due to bugs when working with unsafe code, such as when calling Erlang
/// function.
///
/// This function will catch any error thrown and convert it into a result
/// rather than crashing the process.
///
pub external fn rescue(fn() -> a) -> Result(a, Crash) =
  "gleam_erlang_ffi" "rescue"

pub type Crash {
  Exited(Dynamic)
  Thrown(Dynamic)
  Errored(Dynamic)
}
