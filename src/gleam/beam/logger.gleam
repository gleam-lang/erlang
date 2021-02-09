// Default setting is false for SASL compatible https://erlang.org/doc/man/kernel_app.html#logger_sasl_compatible
import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/map.{Map}
import gleam/result
import gleam/io
import gleam/beam.{ExitReason, Stacktrace}

// These are the log levels used in the erlang logger
// https://erlang.org/doc/apps/kernel/logger_chapter.html#log-level
// They match with a set defined in the Syslog Protocol, RFC 5424
pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

pub fn cast_log_level(raw) {
  let emergency = atom.create_from_string("emergency")
  let alert = atom.create_from_string("alert")
  let critical = atom.create_from_string("critical")
  let error = atom.create_from_string("error")
  let warning = atom.create_from_string("warning")
  let notice = atom.create_from_string("notice")
  let info = atom.create_from_string("info")
  let debug = atom.create_from_string("debug")
  try atom = dynamic.atom(raw)

  case atom {
    a if a == emergency -> Ok(Emergency)
    a if a == alert -> Ok(Alert)
    a if a == critical -> Ok(Critical)
    a if a == error -> Ok(Error)
    a if a == warning -> Ok(Warning)
    a if a == notice -> Ok(Notice)
    a if a == info -> Ok(Info)
    a if a == debug -> Ok(Debug)
  }
  // _ -> result.Error("Not a valid logger level")
}

// Erl log event is this or string or format plus args
pub type Event {
  Event(level: Level, meta: Map(Atom, Dynamic), message: Message)
}

// https://erlang.org/doc/man/logger.html#HModule:log-2
// https://erlang.org/doc/man/logger.html#type-log_event
pub fn cast_log_event(raw) {
  try level = dynamic.field(raw, atom.create_from_string("level"))
  try level = cast_log_level(level)

  try meta = dynamic.field(raw, atom.create_from_string("meta"))
  // Documentation says this field must be in format Map(Atom, term)
  let metadata: Map(Atom, Dynamic) = dynamic.unsafe_coerce(meta)

  try msg = dynamic.field(raw, atom.create_from_string("msg"))
  try message = cast_log_message(msg)
  Event(level, metadata, message)
  |> Ok
}

pub type Message {
  Format(string: String, args: List(Dynamic))
  Report(Dynamic)
  // Would be nice to call this field String as that is how it's defined
  Str(String)
}

pub fn cast_log_message(raw) {
  try first = dynamic.element(raw, 0)
  try second = dynamic.element(raw, 1)

  let report = atom.create_from_string("report")
  let string = atom.create_from_string("string")

  case dynamic.atom(first) {
    Ok(k) if k == report -> Ok(Report(second))
    Ok(k) if k == string -> {
      try str = dynamic.string(second)
      Ok(Str(str))
    }
    _ -> {
      try format_str = dynamic.string(first)
      try args = dynamic.list(second)
      Ok(Format(format_str, args))
    }
  }
}

external fn erl_add_handler(Atom, Atom, Map(Atom, Dynamic)) -> Dynamic =
  "logger" "add_handler"

pub fn add_handler(handler: fn(ExitReason, Stacktrace, Int) -> Nil) -> Nil {
  let handler_module = atom.create_from_string("gleam@beam@logger@handler")
  let config =
    map.from_list([
      tuple(atom.create_from_string("handler"), dynamic.from(handler)),
      tuple(
        atom.create_from_string("level"),
        dynamic.from(atom.create_from_string("error")),
      ),
    ])
  erl_add_handler(handler_module, handler_module, config)
  |> io.debug
  Nil
}
