import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/list
import gleam/map
import gleam/result
import gleam/int
import gleam/io
import gleam/os
import gleam/string
import gleam/beam
import gleam/beam
import gleam/beam/logger.{Event, Report}

// TODO what happens if exit normal
fn do_log(event, config) {
  try Event(level, metadata, message) = logger.cast_log_event(event)
  assert Ok(handler) = map.get(config, atom.create_from_string("handler"))
  let timestamp =
    map.get(metadata, atom.create_from_string("time"))
    |> result.map_error(fn(_) { "time should be an integer" })
    |> result.then(dynamic.int)
    // timestamp is always added by logger to metadata
    // Include system_time call as fallback
    |> result.lazy_unwrap(fn() { os.system_time(os.Microsecond) })

  let timestamp = timestamp / 1000000

  case message {
    Report(report) -> {
      try tuple(reason, stacktrace) = cast_proc_lib_report(report)
      handler(reason, stacktrace, timestamp)
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
  Ok(Nil)
}

pub fn log(event, config) {
  assert Ok(_) = do_log(event, config)
  Nil
}

pub fn cast_proc_lib_report(raw) {
  try report = dynamic.field(raw, atom.create_from_string("report"))
  try [report, _linked] = dynamic.list(report)
  try report = dynamic.typed_list(report, dynamic.tuple2)
  try error_info =
    list.key_find(report, dynamic.from(atom.create_from_string("error_info")))
    |> result.map_error(fn(_) { "Missing error_info key" })

  try kind = dynamic.element(error_info, 0)
  try kind = dynamic.atom(kind)

  let error = atom.create_from_string("error")
  // Other kinds are catch and exit. 
  // Exit is same as error, with no stacktrace
  // catch should be an error with no catch
  case kind {
    k if k == error -> {
      try reason = dynamic.element(error_info, 1)
      try reason = beam.cast_exit_reason(reason)
      try stacktrace = dynamic.element(error_info, 2)
      try stacktrace = beam.cast_stacktrace(stacktrace)
      tuple(reason, stacktrace)
      |> Ok
    }
  }
}
