
import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/function.{rescue}
import gleam/int
import gleam/io
import gleam/list
import gleam/map
import gleam/result
import gleam/string
import gleam/beam/charlist.{Charlist}

external fn erl_format(String, List(a)) -> Charlist =
  "io_lib" "format"

/// Return a string representation of any term
pub fn format(term) {
  charlist.to_string(erl_format("~p", [term]))
}

pub external fn term_to_binary(a) -> BitString =
  "erlang" "term_to_binary"

type Safe {
  Safe
}

external fn erl_binary_to_term(BitString, List(Safe)) -> Dynamic =
  "erlang" "binary_to_term"

pub fn binary_to_term(binary) {
  case rescue(fn() { erl_binary_to_term(binary, [Safe]) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

pub fn unsafe_binary_to_term(binary) {
  case rescue(fn() { erl_binary_to_term(binary, []) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

/// All reasons an that an erlang process might by the runtime. 
///
/// http://erlang.org/documentation/doc-9.3/doc/reference_manual/errors.html#exit_reasons
/// Note `erlang:exit` and `erlang:error` can be called with any term.
/// Therefore when captureing errors always make sure to safely cast to this type, i.e. using `cast_exit_reason`
pub type ExitReason {
  Badarg
  Badarith
  Badmatch(Dynamic)
  FunctionClause
  CaseClause(Dynamic)
  IfClause
  TryClause(Dynamic)
  Undef
  Badfun(Dynamic)
  Badarity(Dynamic)
  TimeoutValue
  Noproc
  Nocatch(Dynamic)
  SystemLimit
}

/// A stacktrace data structure 
pub type Stacktrace =
  List(tuple(Atom, String, Int, String, Int))

/// Safely transform dynamic to exit reason type 
pub fn cast_exit_reason(raw) {
  let badarg = dynamic.from(atom.create_from_string("badarg"))
  let badarith = dynamic.from(atom.create_from_string("badarith"))
  let badmatch = dynamic.from(atom.create_from_string("badmatch"))
  let function_clause = dynamic.from(atom.create_from_string("function_clause"))
  let case_clause = dynamic.from(atom.create_from_string("case_clause"))
  let if_clause = dynamic.from(atom.create_from_string("if_clause"))
  let try_clause = dynamic.from(atom.create_from_string("try_clause"))
  let undef = dynamic.from(atom.create_from_string("undef"))
  let badfun = dynamic.from(atom.create_from_string("badfun"))
  let badarity = dynamic.from(atom.create_from_string("badarity"))
  let timeout_value = dynamic.from(atom.create_from_string("timeout_value"))
  let noproc = dynamic.from(atom.create_from_string("noproc"))
  let nocatch = dynamic.from(atom.create_from_string("nocatch"))
  let system_limit = dynamic.from(atom.create_from_string("system_limit"))

  let key =
    dynamic.element(raw, 0)
    |> result.unwrap(raw)
  case key, dynamic.element(raw, 1) {
    k, Error(_) if k == badarg -> Ok(Badarg)
    k, Error(_) if k == badarith -> Ok(Badarith)
    k, Ok(term) if k == badmatch -> Ok(Badmatch(term))
    k, Error(_) if k == function_clause -> Ok(FunctionClause)
    k, Ok(term) if k == case_clause -> Ok(CaseClause(term))
    k, Error(_) if k == if_clause -> Ok(IfClause)
    k, Ok(term) if k == try_clause -> Ok(TryClause(term))
    k, Error(_) if k == undef -> Ok(Undef)
    k, Ok(term) if k == badfun -> Ok(Badfun(term))
    k, Ok(term) if k == badarity -> Ok(Badarity(term))
    k, Error(_) if k == timeout_value -> Ok(TimeoutValue)
    k, Error(_) if k == noproc -> Ok(Noproc)
    k, Ok(term) if k == nocatch -> Ok(Nocatch(term))
    k, Error(_) if k == system_limit -> Ok(SystemLimit)
    _, _ -> Error("Not a runtime exit reason")
  }
}

/// Safely cast a dynamic stacktrace to typed data.
pub fn cast_stacktrace(raw) -> Result(Stacktrace, String) {
  try raw_frames = dynamic.list(raw)
  list.try_map(raw_frames, cast_stack_frame)
}

// https://github.com/elixir-lang/elixir/blob/76d245b6081c53228bf99fc1494add5de7872065/lib/elixir/lib/exception.ex#L28
// stacktrace is module, function (atom or charlist), args_or_arity, location(keyword list)
fn cast_stack_frame(raw) {
  try module =
    dynamic.element(raw, 0)
    |> result.then(dynamic.atom)

  try function_raw = dynamic.element(raw, 1)
  let function = case dynamic.atom(function_raw) {
    Ok(function) -> atom.to_string(function)
    Error(_) -> charlist.to_string(dynamic.unsafe_coerce(function_raw))
  }

  try arity_raw = dynamic.element(raw, 2)
  let arity = case dynamic.int(arity_raw) {
    Ok(arity) -> arity
    Error(_) -> list.length(dynamic.unsafe_coerce(arity_raw))
  }

  try location =
    dynamic.element(raw, 3)
    |> result.then(dynamic.typed_list(_, dynamic.tuple2))
    |> result.map(map.from_list)

  let file_atom = dynamic.from(atom.create_from_string("file"))
  let line_atom = dynamic.from(atom.create_from_string("line"))

  try filename =
    map.get(location, file_atom)
    |> result.map_error(fn(_: Nil) { "Missing key 'file'" })

  let filename = charlist.to_string(dynamic.unsafe_coerce(filename))

  try line_number =
    map.get(location, line_atom)
    |> result.map_error(fn(_: Nil) { "Missing key 'line'" })
    |> result.then(dynamic.int)

  Ok(tuple(module, function, arity, filename, line_number))
}
