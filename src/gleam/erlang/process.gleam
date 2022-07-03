// TODO: changelog
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

// TODO: changelog
// TODO: test
/// Create a new unique reference.
///
pub external fn make_reference() -> Reference =
  "erlang" "make_ref"

// TODO: changelog
/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub external type Pid

// TODO: changelog
/// Get the `Pid` for the current process.
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: changelog
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
pub fn start(linked link: Bool, running implementation: fn() -> anything) -> Pid {
  case link {
    True -> spawn_link(implementation)
    False -> spawn(implementation)
  }
}

external fn spawn(fn() -> anything) -> Pid =
  "erlang" "spawn"

external fn spawn_link(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

// TODO: changelog
// TODO: document
pub opaque type Subject(message) {
  Subject(owner: Pid, tag: Reference)
}

// TODO: changelog
// TODO: document
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), tag: make_reference())
}

// TODO: changelog
/// Get the owner process for a `Subject`. This is the process that will receive
/// messages sent via the `Subject`.
///
pub fn subject_owner(subject: Subject(message)) -> Pid {
  subject.owner
}

external type DoNotLeak

external fn raw_send(Pid, message) -> DoNotLeak =
  "erlang" "send"

// TODO: document
// TODO: test
// TODO: changelog
pub fn send(subject: Subject(message), message: message) -> Nil {
  raw_send(subject.owner, #(subject.tag, message))
  Nil
}

// TODO: changelog
// TODO: test
// TODO: document
pub fn receive(
  from subject: Subject(message),
  within milliseconds: Int,
) -> Result(message, Nil) {
  new_selector()
  |> selecting(subject, fn(x) { x })
  |> select(within: milliseconds)
}

// TODO: changelog
// TODO: document
pub external type Selector(payload)

// TODO: changelog
// TODO: document
pub external fn new_selector() -> Selector(payload) =
  "gleam_erlang_ffi" "new_selector"

// TODO: changelog
// TODO: test
// TODO: document
pub external fn select(
  from: Selector(payload),
  within: Int,
) -> Result(payload, Nil) =
  "gleam_erlang_ffi" "select"

// TODO: changelog
// TODO: document
pub external fn selecting(
  Selector(payload),
  for: Subject(message),
  mapping: fn(message) -> payload,
) -> Selector(payload) =
  "gleam_erlang_ffi" "selecting"

// TODO: changelog
/// Suspends the process calling this function for the specified number of
/// milliseconds.
///
pub external fn sleep(Int) -> Nil =
  "gleam_erlang_ffi" "sleep"

// TODO: changelog
/// Suspends the process forever! This may be useful for suspending the main
/// process in a Gleam program when it has no more work to do but we want other
/// processes to continue to work.
///
pub external fn sleep_forever() -> Nil =
  "gleam_erlang_ffi" "sleep_forever"
