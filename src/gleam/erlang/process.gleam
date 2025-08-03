import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/port.{type Port}
import gleam/erlang/reference.{type Reference}
import gleam/string

/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub type Pid

/// Get the `Pid` for the current process.
///
@external(erlang, "erlang", "self")
pub fn self() -> Pid

/// Create a new Erlang process that runs concurrently to the creator. In other
/// languages this might be called a fibre, a green thread, or a coroutine.
///
/// The child process is linked to the creator process. When a process
/// terminates an exit signal is sent to all other processes that are linked to
/// it, causing the process to either terminate or have to handle the signal.
/// If you want an unlinked process use the `spawn_unlinked` function.
///
/// More can be read about processes and links in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/reference_manual/processes.html
///
/// This function starts processes via the Erlang `proc_lib` module, and as
/// such they benefit from the functionality described in the
/// [`proc_lib` documentation](https://www.erlang.org/doc/apps/stdlib/proc_lib.html).
///
@external(erlang, "proc_lib", "spawn_link")
pub fn spawn(running: fn() -> anything) -> Pid

/// Create a new Erlang process that runs concurrently to the creator. In other
/// languages this might be called a fibre, a green thread, or a coroutine.
///
/// Typically you want to create a linked process using the `spawn` function,
/// but creating an unlinked process may be occasionally useful.
///
/// More can be read about processes and links in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/reference_manual/processes.html
///
/// This function starts processes via the Erlang `proc_lib` module, and as
/// such they benefit from the functionality described in the
/// [`proc_lib` documentation](https://www.erlang.org/doc/apps/stdlib/proc_lib.html).
///
@external(erlang, "proc_lib", "spawn")
pub fn spawn_unlinked(a: fn() -> anything) -> Pid

/// A `Subject` is a value that processes can use to send and receive messages
/// to and from each other in a well typed way.
///
/// Each subject is "owned" by the process that created it. Any process can use
/// the `send` function to send a message of the correct type to the process
/// that owns the subject, and the owner can use the `receive` function or the
/// `Selector` type to receive these messages.
///
/// The `Subject` type is similar to the "channel" types found in other
/// languages and the "topic" concept found in some pub-sub systems.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
///
/// // Send a message with the subject
/// send(subject, "Hello, Joe!")
///
/// // Receive the message
/// receive(subject, within: 10)
/// ```
///
pub opaque type Subject(message) {
  Subject(owner: Pid, tag: Dynamic)
  NamedSubject(name: Name(message))
}

/// Create a subject for the given process with the give tag. This is unsafe!
/// There's nothing here that verifies that the message the subject receives is
/// expected and that the tag is not already in use.
///
/// You should almost certainly not use this function.
///
@internal
pub fn unsafely_create_subject(owner: Pid, tag: Dynamic) -> Subject(message) {
  Subject(owner, tag)
}

/// A name is an identity that a process can adopt, after which they will receive
/// messages sent to that name. This has two main advantages:
///
/// - Structuring OTP programs becomes easier as a name can be passed down the
///   program from the top level, while without names subjects and pids would
///   need to be passed up from the started process and then back down to the
///   code that works with that process.
/// - A new process can adopt the name of one that previously failed, allowing
///   it to transparently take-over and handle messages that are sent to that
///   name.
///
/// Names are globally unique as each process can have at most 1 name, and each
/// name can be registered by at most 1 process. Create all the names your
/// program needs at the start of your program and pass them down. Names are
/// Erlang atoms internally, so never create them dynamically. Generating too
/// many atoms will result in the atom table getting filled and causing the entire
/// virtual machine to crash.
///
/// The most commonly used name functions are `new_name`, `register`, and
/// `named_subject`.
///
pub type Name(message)

/// Generate a new name that a process can register itself with using the
/// `register` function, and other processes can send messages to using
/// `named_subject`.
///
/// The string argument is a prefix for the Erlang name. A unique suffix is
/// added to the prefix to make the name, removing the possibility of name
/// collisions.
///
/// ## Safe use
///
/// Use this function to create all the names your program needs when it
/// starts. **Never call this function dynamically** such as within a loop or
/// within a process within a supervision tree.
///
/// Each time this function is called a new atom will be generated. Generating
/// too many atoms will result in the atom table getting filled and causing the
/// entire virtual machine to crash.
///
@external(erlang, "gleam_erlang_ffi", "new_name")
pub fn new_name(prefix prefix: String) -> Name(message)

/// Create a subject for a name, which can be used to send and receive messages.
///
/// All subjects created for the same name behave identically and can be used
/// interchangably.
///
pub fn named_subject(name: Name(message)) -> Subject(message) {
  NamedSubject(name)
}

/// Get the name of a subject, returning an error if it doesn't have one.
///
pub fn subject_name(subject: Subject(message)) -> Result(Name(message), Nil) {
  case subject {
    NamedSubject(name:) -> Ok(name)
    Subject(..) -> Error(Nil)
  }
}

/// Create a new `Subject` owned by the current process.
///
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), tag: reference_to_dynamic(reference.new()))
}

/// Get the owner process for a subject, which is the process that will
/// receive any messages sent using the subject.
///
/// If the subject was created from a name and no process is currently
/// registered with that name then this function will return an error.
///
pub fn subject_owner(subject: Subject(message)) -> Result(Pid, Nil) {
  case subject {
    NamedSubject(name) -> named(name)
    Subject(pid, _) -> Ok(pid)
  }
}

type DoNotLeak

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: message) -> DoNotLeak

/// Send a message to a process using a `Subject`. The message must be of the
/// type that the `Subject` accepts.
///
/// This function does not wait for the `Subject` owner process to call the
/// `receive` function, instead it returns once the message has been placed in
/// the process' mailbox.
/// 
/// # Named Subjects
/// 
/// If this function is called on a named subject for which a process has not been 
/// registered, it will simply drop the message as there's no mailbox to send it to.
///
/// # Panics
///
/// This function will panic when sending to a named subject if no process is
/// currently registed under that name.
///
/// # Ordering
///
/// If process P1 sends two messages to process P2 it is guaranteed that process
/// P1 will receive the messages in the order they were sent.
///
/// If you wish to receive the messages in a different order you can send them
/// on two different subjects and the receiver function can call the `receive`
/// function for each subject in the desired order, or you can write some Erlang
/// code to perform a selective receive.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
/// send(subject, "Hello, Joe!")
/// ```
///
pub fn send(subject: Subject(message), message: message) -> Nil {
  case subject {
    Subject(pid, tag) -> {
      raw_send(pid, #(tag, message))
    }
    NamedSubject(name) -> {
      let assert Ok(pid) = named(name) as "Sending to unregistered name"
      raw_send(pid, #(name, message))
    }
  }
  Nil
}

/// Receive a message that has been sent to current process using the `Subject`.
///
/// If there is not an existing message for the `Subject` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject` can receive a message using
/// it. If a process that does not own the `Subject` attempts to receive with it
/// then it will not receive a message.
///
/// To wait for messages from multiple `Subject`s at the same time see the
/// `Selector` type.
///
/// The `within` parameter specifies the timeout duration in milliseconds.
///
/// ## Panics
///
/// This function will panic if a process tries to receive with a non-named
/// subject that it does not own.
///
pub fn receive(
  from subject: Subject(message),
  within timeout: Int,
) -> Result(message, Nil) {
  case subject {
    NamedSubject(..) -> perform_receive(subject, timeout)
    Subject(owner:, ..) ->
      case owner == self() {
        True -> perform_receive(subject, timeout)
        False ->
          panic as "Cannot receive with a subject owned by another process"
      }
  }
}

@external(erlang, "gleam_erlang_ffi", "receive")
fn perform_receive(
  subject: Subject(message),
  timeout: Int,
) -> Result(message, Nil)

/// Receive a message that has been sent to current process using the `Subject`.
///
/// Same as `receive` but waits forever and returns the message as is.
@external(erlang, "gleam_erlang_ffi", "receive")
pub fn receive_forever(from subject: Subject(message)) -> message

/// A type that enables a process to wait for messages from multiple `Subject`s
/// at the same time, returning whichever message arrives first.
///
/// Used with the `new_selector`, `selector_receive`, and `select*` functions.
///
/// # Examples
///
/// ```gleam
/// let int_subject = new_subject()
/// let string_subject = new_subject()
/// send(int_subject, 1)
///
/// let selector =
///   new_selector()
///   |> select(string_subject)
///   |> select_map(int_subject, int.to_string)
///
/// selector_receive(selector, 10)
/// // -> Ok("1")
/// ```
///
pub type Selector(payload)

/// Create a new `Selector` which can be used to receive messages on multiple
/// `Subject`s at once.
///
@external(erlang, "gleam_erlang_ffi", "new_selector")
pub fn new_selector() -> Selector(payload)

/// Receive a message that has been sent to current process using any of the
/// `Subject`s that have been added to the `Selector` with the `select*`
/// functions.
///
/// If there is not an existing message for the `Selector` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject`s can receive a message using
/// them. If a process that does not own the a `Subject` attempts to receive
/// with it then it will not receive a message.
///
/// To wait forever for the next message rather than for a limited amount of
/// time see the `selector_receive_forever` function.
///
/// The `within` parameter specifies the timeout duration in milliseconds.
///
@external(erlang, "gleam_erlang_ffi", "select")
pub fn selector_receive(
  from from: Selector(payload),
  within within: Int,
) -> Result(payload, Nil)

/// Similar to the `select` function but will wait forever for a message to
/// arrive rather than timing out after a specified amount of time.
///
@external(erlang, "gleam_erlang_ffi", "select")
pub fn selector_receive_forever(from from: Selector(payload)) -> payload

/// Add a transformation function to a selector. When a message is received
/// using this selector the transformation function is applied to the message.
///
/// This function can be used to change the type of messages received and may
/// be useful when combined with the `merge_selector` function.
///
@external(erlang, "gleam_erlang_ffi", "map_selector")
pub fn map_selector(a: Selector(a), b: fn(a) -> b) -> Selector(b)

/// Merge one selector into another, producing a selector that contains the
/// message handlers of both.
///
/// If a subject is handled by both selectors the handler function of the
/// second selector is used.
///
@external(erlang, "gleam_erlang_ffi", "merge_selector")
pub fn merge_selector(a: Selector(a), b: Selector(a)) -> Selector(a)

pub type ExitMessage {
  ExitMessage(pid: Pid, reason: ExitReason)
}

pub type ExitReason {
  Normal
  Killed
  Abnormal(reason: Dynamic)
}

/// Add a handler for trapped exit messages. In order for these messages to be
/// sent to the process when a linked process exits the process must call the
/// `trap_exit` beforehand.
///
pub fn select_trapped_exits(
  selector: Selector(a),
  handler: fn(ExitMessage) -> a,
) -> Selector(a) {
  let tag = atom.create("EXIT")
  let handler = fn(message: #(Atom, Pid, Dynamic)) -> a {
    handler(ExitMessage(message.1, cast_exit_reason(message.2)))
  }
  insert_selector_handler(selector, #(tag, 3), handler)
}

/// Discard all messages in the current process' mailbox.
///
/// Warning: This function may cause other processes to crash if they sent a
/// message to the current process and are waiting for a response, so use with
/// caution.
///
/// This function may be useful in tests.
///
@external(erlang, "gleam_erlang_ffi", "flush_messages")
pub fn flush_messages() -> Nil

/// Add a new `Subject` to the `Selector` so that its messages can be selected
/// from the receiver process inbox.
///
/// See `select_map` to add subjects of a different message type.
///
/// See `deselect` to remove a subject from a selector.
///
pub fn select(
  selector: Selector(payload),
  for subject: Subject(payload),
) -> Selector(payload) {
  select_map(selector, subject, fn(x) { x })
}

/// Add a new `Subject` to the `Selector` so that its messages can be selected
/// from the receiver process inbox.
///
/// The `mapping` function provided with the `Subject` can be used to convert
/// the type of messages received using this `Subject`. This is useful for when
/// you wish to add multiple `Subject`s to a `Selector` when they have differing
/// message types. If you do not wish to transform the incoming messages in any
/// way then the `identity` function can be given.
///
/// See `deselect` to remove a subject from a selector.
///
pub fn select_map(
  selector: Selector(payload),
  for subject: Subject(message),
  mapping transform: fn(message) -> payload,
) -> Selector(payload) {
  let handler = fn(message: #(Reference, message)) { transform(message.1) }
  case subject {
    NamedSubject(name) -> insert_selector_handler(selector, #(name, 2), handler)
    Subject(_, tag:) -> insert_selector_handler(selector, #(tag, 2), handler)
  }
}

/// Remove a new `Subject` from the `Selector` so that its messages will not be
/// selected from the receiver process inbox.
///
pub fn deselect(
  selector: Selector(payload),
  for subject: Subject(message),
) -> Selector(payload) {
  case subject {
    NamedSubject(name) -> remove_selector_handler(selector, #(name, 2))
    Subject(_, tag:) -> remove_selector_handler(selector, #(tag, 2))
  }
}

/// Add a handler to a selector for tuple messages with a given tag in the
/// first position followed by a given number of fields.
///
/// Typically you want to use the `select` function with a `Subject` instead,
/// but this function may be useful if you need to receive messages sent from
/// other BEAM languages that do not use the `Subject` type.
///
/// This will not select messages sent via a subject even if the message has
/// the same tag in the first position. This is because when a message is sent
/// via a subject a new tag is used that is unique and specific to that subject.
///
pub fn select_record(
  selector: Selector(payload),
  tag tag: tag,
  fields arity: Int,
  mapping transform: fn(Dynamic) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, #(tag, arity + 1), transform)
}

type AnythingSelectorTag {
  Anything
}

/// Add a catch-all handler to a selector that will be used when no other
/// handler in a selector is suitable for a given message.
///
/// This may be useful for when you want to ensure that any message in the inbox
/// is handled, or when you need to handle messages from other BEAM languages
/// which do not use subjects or record format messages.
///
pub fn select_other(
  selector: Selector(payload),
  mapping handler: fn(Dynamic) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, Anything, handler)
}

@external(erlang, "gleam_erlang_ffi", "insert_selector_handler")
fn insert_selector_handler(
  a: Selector(payload),
  for for: tag,
  mapping mapping: fn(message) -> payload,
) -> Selector(payload)

@external(erlang, "gleam_erlang_ffi", "remove_selector_handler")
fn remove_selector_handler(
  a: Selector(payload),
  for for: tag,
) -> Selector(payload)

/// Suspends the process calling this function for the specified number of
/// milliseconds.
///
@external(erlang, "gleam_erlang_ffi", "sleep")
pub fn sleep(a: Int) -> Nil

/// Suspends the process forever! This may be useful for suspending the main
/// process in a Gleam program when it has no more work to do but we want other
/// processes to continue to work.
///
@external(erlang, "gleam_erlang_ffi", "sleep_forever")
pub fn sleep_forever() -> Nil

/// Check to see whether the process for a given `Pid` is alive.
///
/// See the [Erlang documentation][1] for more information.
///
/// [1]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
@external(erlang, "erlang", "is_process_alive")
pub fn is_alive(a: Pid) -> Bool

type ProcessMonitorFlag {
  Process
}

@external(erlang, "erlang", "monitor")
fn erlang_monitor_process(a: ProcessMonitorFlag, b: Pid) -> Monitor

pub type Monitor

/// A message received when a monitored process exits.
///
pub type Down {
  ProcessDown(monitor: Monitor, pid: Pid, reason: ExitReason)
  PortDown(monitor: Monitor, port: Port, reason: ExitReason)
}

/// Start monitoring a process so that when the monitored process exits a
/// message is sent to the monitoring process.
///
/// The message is only sent once, when the target process exits. If the
/// process was not alive when this function is called the message will never
/// be received.
///
/// The down message can be received with a selector and the
/// `select_monitors` function.
///
/// The process can be demonitored with the `demonitor_process` function.
///
pub fn monitor(pid: Pid) -> Monitor {
  erlang_monitor_process(Process, pid)
}

/// Select for a message sent for a given monitor.
///
/// Each monitor handler added to a selector has a select performance cost,
/// so prefer [`select_monitors`](#select_monitors) if you are select
/// for multiple monitors.
///
/// The handler can be removed from the selector later using
/// [`deselect_specific_monitor`](#deselect_specific_monitor).
///
pub fn select_specific_monitor(
  selector: Selector(payload),
  monitor: Monitor,
  mapping: fn(Down) -> payload,
) {
  insert_selector_handler(selector, monitor, mapping)
}

/// Select for any messages sent for any monitors set up by the select process.
///
/// If you want to select for a specific message then use 
/// [`select_specific_monitor`](#select_specific_monitor), but this
/// function is preferred if you need to select for multiple monitors.
///
pub fn select_monitors(
  selector: Selector(payload),
  mapping: fn(Down) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, #(atom.create("DOWN"), 5), fn(message) {
    mapping(cast_down_message(message))
  })
}

@external(erlang, "gleam_erlang_ffi", "cast_down_message")
fn cast_down_message(message: Dynamic) -> Down

@external(erlang, "gleam_erlang_ffi", "cast_exit_reason")
fn cast_exit_reason(message: Dynamic) -> ExitReason

/// Remove the monitor for a process so that when the monitor process exits a
/// `Down` message is not sent to the monitoring process.
///
/// If the message has already been sent it is removed from the monitoring
/// process' mailbox.
///
pub fn demonitor_process(monitor monitor: Monitor) -> Nil {
  erlang_demonitor_process(monitor)
  Nil
}

@external(erlang, "gleam_erlang_ffi", "demonitor")
fn erlang_demonitor_process(monitor: Monitor) -> DoNotLeak

/// Remove a `Monitor` from a `Selector` prevoiusly added by
/// [`select_specific_monitor`](#select_specific_monitor). If
/// the `Monitor` is not in the `Selector` it will be returned
/// unchanged.
///
pub fn deselect_specific_monitor(
  selector: Selector(payload),
  monitor: Monitor,
) -> Selector(payload) {
  remove_selector_handler(selector, monitor)
}

fn perform_call(
  subject: Subject(message),
  make_request: fn(Subject(reply)) -> message,
  run_selector: fn(Selector(reply)) -> Result(reply, Nil),
) -> reply {
  let reply_subject = new_subject()
  let assert Ok(callee) = subject_owner(subject)
    as "Callee subject had no owner"

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor = monitor(callee)

  // Send the request to the process over the channel
  send(subject, make_request(reply_subject))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let reply =
    new_selector()
    |> select(reply_subject)
    |> select_specific_monitor(monitor, fn(down) {
      panic as { "callee exited: " <> string.inspect(down) }
    })
    |> run_selector

  let assert Ok(reply) = reply as "callee did not send reply before timeout"

  // Demonitor the process and close the channels as we're done
  demonitor_process(monitor)

  reply
}

// This function is based off of Erlang's gen:do_call/4.
/// Send a message to a process and wait a given number of milliseconds for a
/// reply.
///
/// ## Panics
///
/// This function will panic under the following circumstances:
/// - The callee process exited prior to sending a reply.
/// - The callee process did not send a reply within the permitted amount of
///   time.
/// - The subject is a named subject but no process is registered with that
///   name.
///
/// ## Examples
///
/// ```gleam
/// pub type Message {
///   // This message variant is to be used with `call`.
///   // The `reply` field contains a subject that the reply message will be
///   // sent over.
///   SayHello(reply_to: Subject(String), name: String)
/// }
/// 
/// // Typically we make public functions that hide the details of a process'
/// // message-based API.
/// pub fn say_hello(subject: Subject(Message), name: String) -> String {
///   // The `SayHello` message constructor is given _partially applied_ with
///   // all the arguments except the reply subject, which will be supplied by
///   // the `call` function itself before sending the message.
///   process.call(subject, 100, SayHello(_, name))
/// }
///
/// // This is the message handling logic used by the process that owns the
/// // subject, and so receives the messages. In a real project it would be
/// // within a process or some higher level abstraction like an actor, but for
/// // this demonstration that has been omitted.
/// pub fn handle_message(message: Message) -> Nil {
///   case message {
///     SayHello(reply:, name:) -> {
///       let data = "Hello, " <> name <> "!"
///       // The reply subject is used to send the response back.
///       // If the receiver process does not sent a reply in time then the
///       // caller will crash.
///       process.send(reply, data)
///     }
///   }
/// }
///
/// // Here is what it looks like using the functional API to call the process.
/// pub fn run(subject: Subject(Message)) {
///   say_hello(subject, "Lucy")
///   // -> "Hello, Lucy!"
///   say_hello(subject, "Nubi")
///   // -> "Hello, Nubi!"
/// }
/// ```
///
pub fn call(
  subject: Subject(message),
  waiting timeout: Int,
  sending make_request: fn(Subject(reply)) -> message,
) -> reply {
  perform_call(subject, make_request, selector_receive(_, timeout))
}

/// Send a message to a process and wait for a reply.
///
/// # Panics
///
/// This function will panic under the following circumstances:
/// - The callee process exited prior to sending a reply.
/// - The subject is a named subject but no process is registered with that
///   name.
///
pub fn call_forever(
  subject: Subject(message),
  make_request: fn(Subject(reply)) -> message,
) -> reply {
  perform_call(subject, make_request, fn(s) { Ok(selector_receive_forever(s)) })
}

/// Creates a link between the calling process and another process.
///
/// When a process crashes any linked processes will also crash. This is useful
/// to ensure that groups of processes that depend on each other all either
/// succeed or fail together.
///
/// Returns `True` if the link was created successfully, returns `False` if the
/// process was not alive and as such could not be linked.
///
@external(erlang, "gleam_erlang_ffi", "link")
pub fn link(pid pid: Pid) -> Bool

@external(erlang, "erlang", "unlink")
fn erlang_unlink(pid pid: Pid) -> Bool

/// Removes any existing link between the caller process and the target process.
///
pub fn unlink(pid: Pid) -> Nil {
  erlang_unlink(pid)
  Nil
}

pub type Timer

@external(erlang, "erlang", "send_after")
fn pid_send_after(a: Int, b: Pid, c: #(Dynamic, msg)) -> Timer

@external(erlang, "erlang", "send_after")
fn name_send_after(a: Int, b: Name(msg), c: #(Name(msg), msg)) -> Timer

/// Send a message over a channel after a specified number of milliseconds.
///
pub fn send_after(subject: Subject(msg), delay: Int, message: msg) -> Timer {
  case subject {
    NamedSubject(name) -> name_send_after(delay, name, #(name, message))
    Subject(owner, tag) -> pid_send_after(delay, owner, #(tag, message))
  }
}

@external(erlang, "gleam_erlang_ffi", "identity")
fn reference_to_dynamic(reference: Reference) -> Dynamic

@external(erlang, "erlang", "cancel_timer")
fn erlang_cancel_timer(a: Timer) -> Dynamic

/// Values returned when a timer is cancelled.
///
pub type Cancelled {
  /// The timer could not be found. It likely has already triggered.
  ///
  TimerNotFound

  /// The timer was found and cancelled before it triggered.
  ///
  /// The amount of remaining time before the timer was due to be triggered is
  /// returned in milliseconds.
  ///
  Cancelled(time_remaining: Int)
}

/// Cancel a given timer, causing it not to trigger if it has not done already.
///
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case decode.run(erlang_cancel_timer(timer), decode.int) {
    Ok(i) -> Cancelled(i)
    Error(_) -> TimerNotFound
  }
}

type KillFlag {
  Kill
}

@external(erlang, "erlang", "exit")
fn erlang_kill(to to: Pid, because because: KillFlag) -> Bool

// Go, my pretties. Kill! Kill!
// - Bart Simpson
//
/// Send an untrappable `kill` exit signal to the target process.
///
/// See the documentation for the Erlang [`erlang:exit`][1] function for more
/// information.
///
/// [1]: https://erlang.org/doc/man/erlang.html#exit-1
///
pub fn kill(pid: Pid) -> Nil {
  erlang_kill(pid, Kill)
  Nil
}

@external(erlang, "erlang", "exit")
fn erlang_send_exit(to to: Pid, because because: whatever) -> Bool

// TODO: test
/// Sends an exit signal to a process, indicating that the process is to shut
/// down.
///
/// See the [Erlang documentation][1] for more information.
///
/// [1]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_exit(to pid: Pid) -> Nil {
  erlang_send_exit(pid, Normal)
  Nil
}

/// Sends an exit signal to a process, indicating that the process is to shut
/// down due to an abnormal reason such as a failure.
///
/// See the [Erlang documentation][1] for more information.
///
/// [1]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_abnormal_exit(pid: Pid, reason: anything) -> Nil {
  erlang_send_exit(pid, reason)
  Nil
}

/// Set whether the current process is to trap exit signals or not.
///
/// When not trapping exits if a linked process crashes the exit signal
/// propagates to the process which will also crash.
/// This is the normal behaviour before this function is called.
///
/// When trapping exits (after this function is called) if a linked process
/// crashes an exit message is sent to the process instead. These messages can
/// be handled with the `select_trapped_exits` function.
///
@external(erlang, "gleam_erlang_ffi", "trap_exits")
pub fn trap_exits(a: Bool) -> Nil

/// Register a process under a given name, allowing it to be looked up using
/// the `named` function.
///
/// This function will return an error under the following conditions:
/// - The process for the pid no longer exists.
/// - The name has already been registered.
/// - The process already has a name.
///
@external(erlang, "gleam_erlang_ffi", "register_process")
pub fn register(pid: Pid, name: Name(message)) -> Result(Nil, Nil)

/// Un-register a process name, after which the process can no longer be looked
/// up by that name, and both the name and the process can be re-used in other
/// registrations.
///
/// It is possible to un-register process that are not from your application,
/// including those from Erlang/OTP itself. This is not recommended and will
/// likely result in undesirable behaviour and crashes.
///
@external(erlang, "gleam_erlang_ffi", "unregister_process")
pub fn unregister(name: Name(message)) -> Result(Nil, Nil)

/// Look up a process by registered name, returning the pid if it exists.
///
@external(erlang, "gleam_erlang_ffi", "process_named")
pub fn named(name: Name(a)) -> Result(Pid, Nil)
