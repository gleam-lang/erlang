import gleam/dynamic.{DecodeError}
import gleam/erlang.{UnknownApplication}
import gleam/erlang/atom
import gleam/iterator
import gleam/string

pub fn term_to_binary_test() {
  let term = dynamic.from(#(1, "2", <<"hello":utf8>>))

  let assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.binary_to_term()
  let assert True = term == out

  let assert Ok(out) =
    term
    |> erlang.term_to_binary()
    |> erlang.unsafe_binary_to_term()
  let assert True = term == out

  let assert Error(Nil) =
    <<>>
    |> erlang.unsafe_binary_to_term()
}

pub fn ensure_all_started_ok_test() {
  let inets = atom.create_from_string("inets")
  let assert Ok([app1]) = erlang.ensure_all_started(inets)
  let assert True = app1 == inets

  // If they are already started then empty list is returned
  let assert Ok([]) = erlang.ensure_all_started(inets)
}

pub fn ensure_all_started_unknown_test() {
  let unknown = atom.create_from_string("wibble_application")
  let assert Error(UnknownApplication(problem)) =
    erlang.ensure_all_started(unknown)
  let assert True = problem == unknown
}

pub fn make_reference_test() {
  let reference = erlang.make_reference()
  iterator.range(0, 100_000)
  |> iterator.map(fn(_) {
    let assert True = reference != erlang.make_reference()
  })
  |> iterator.run
}

pub fn reference_from_dynamic_test() {
  let reference = erlang.make_reference()
  let assert Ok(reference_from_dynamic) =
    reference
    |> dynamic.from
    |> erlang.reference_from_dynamic
  let assert True = reference == reference_from_dynamic

  let assert Error([DecodeError(expected: "Reference", found: "Int", path: [])]) =
    123
    |> dynamic.from
    |> erlang.reference_from_dynamic

  let assert Error([
    DecodeError(expected: "Reference", found: "String", path: []),
  ]) =
    "abc"
    |> dynamic.from
    |> erlang.reference_from_dynamic
}

pub fn priv_directory_test() {
  let assert Error(Nil) = erlang.priv_directory("unknown_application")

  let assert Ok(dir) = erlang.priv_directory("gleam_erlang")
  let assert True = string.ends_with(dir, "/gleam_erlang/priv")

  let assert Ok(dir) = erlang.priv_directory("gleam_stdlib")
  let assert True = string.ends_with(dir, "/gleam_stdlib/priv")
}

pub fn bounded_phash2_test() {
  let assert 9 = erlang.bounded_phash2("hello", limit: 10)
  let assert 0 = erlang.bounded_phash2([5, 2, 8], limit: 10)
  let assert 82 = erlang.bounded_phash2(Ok(#("testing", 123)), limit: 200)
}

pub fn phash2_test() {
  let assert 47_480_723 = erlang.phash2("hello")
  let assert 79_761_634 = erlang.phash2([5, 2, 8])
  let assert 133_777_698 = erlang.phash2(Ok(#("testing", 123)))
}
