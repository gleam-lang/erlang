/// A unique reference value.
///
/// It holds no particular meaning or value, but unique values are often useful
/// in programs are used heavily within both Gleam and Erlang's OTP frameworks.
///
/// More can be read about refernces in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/efficiency_guide/advanced.html#unique_references
///
pub external type Reference

// TODO: test
/// Create a new unique reference.
///
pub external fn make_reference() -> Reference =
  "erlang" "make_ref"

/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub external type Pid

// TODO: test
/// Get the `Pid` for the current process.
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: test
/// Create a new Erlang process that runs concurrently to the creator. In other
/// languages this might be called a fibre, a green thread, or a coroutine.
///
/// If `linked` is `True` then the created process is linked to the creator
/// process. When a process terminates an exit signal is sent to all other
/// processes that are linked to it, causing the process to either terminate or
/// have to handle the signal.
///
/// More can be read about processes and links in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/reference_manual/processes.html
///
pub fn start_process(
  linked link: Bool,
  running implementation: fn() -> anything,
) -> Pid {
  case link {
    True -> spawn_link(implementation)
    False -> spawn(implementation)
  }
}

external fn spawn(fn() -> anything) -> Pid =
  "erlang" "spawn"

external fn spawn_link(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

// TODO: document
pub opaque type Subject(message) {
  Subject(owner: Pid, reference: Reference)
}

// TODO: test
// TODO: document
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), reference: make_reference())
}

// TODO: test
/// Get the owner process for a `Subject`. This is the process that will receive
/// messages sent via the `Subject`.
///
pub fn owner(subject: Subject(message)) -> Pid {
  subject.owner
}

// TODO: test
// TODO: document
pub fn receive(
  within milliseconds: Int,
  from subject: Subject(message),
) -> Result(message, Nil) {
  new_selector()
  |> selecting(subject, fn(x) { x })
  |> select(within: milliseconds)
}

// TODO: document
pub external type Selector(payload)

// TODO: document
pub external fn new_selector() -> Selector(payload) =
  "gleam_erlang_ffi" "new_selector"

// TODO: test
// TODO: document
pub external fn select(
  from: Selector(payload),
  within: Int,
) -> Result(payload, Nil) =
  "gleam_erlang_ffi" "select"

// TODO: document
pub external fn selecting(
  Selector(payload),
  for: Subject(message),
  mapping: fn(message) -> payload,
) -> Selector(payload) =
  "gleam_erlang_ffi" "selecting"
