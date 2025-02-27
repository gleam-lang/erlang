# Changelog

## v1.0.0-rc1 - Unreleased

- In the `gleam/erlang/process` module:
  - The `Name` type has been introduced. This type is used to give processes
    names, making it easier to pass references around your application, and to
    have a new process take over a role from a previous one that has crashed.
  - The `new_name` function has been added for creating new names.
  - The `named_subject` function has been added for creating a subject for a
    given name.
  - The `register` function now takes a name rather than an atom.
  - The `unregister` function now takes a name rather than an atom.
  - The `named` function now takes a name rather than an atom.
  - The `try_call` and `try_call_forever` functions have been removed in favour
    of `call` and `call_forever`.
  - The `start` function has been replaced by the `spawn` and `spawn_unlinked`
    functions.
  - The `subject_owner` function now returns a result as a named subject may not
    have any process registered for that name.
- In the `gleam/erlang/node` module:
  - The `to_atom` function has been removed.
  - The `send` function has been removed.
  - The `untyped_send` function has been added. It sends messages in the same
    format as the `send` process from the `gleam/erlang/process` module.
- The `gleam/erlang/reference` module has been created with:
  - The `Reference` type.
  - The `new` function.
- The `gleam/erlang/application` module was created with:
  - The `priv_directory` function, formerly of the `gleam/erlang` module.
  - The `StartType` type.
- In the `gleam/erlang/atom` module:
  - The `AtomNotLoaded` type has been removed.
  - The error type of `from_string` is now `Nil`.
  - The `from_string` function has been renamed to `get`.
  - The `create_from_string` function has been renamed to `create`.
- The `gleam/erlang` module has been removed.
- The `gleam/erlang/os` module has been removed.

## v0.34.0 - 2025-02-02

- Fixed deprecation warnings with the Gleam standard library v0.53.0 or later.
- Increased minimum required Gleam standard library version to v0.53.0.

## v0.33.1 - 2024-12-07

- Fixed a bug where `process.demonitor_process` would return the incorrect
  value.

## v0.33.0 - 2024-12-05

- The `gleam/erlang/process` module gains the `receive_forever` function.
- The `gleam/os` environment functions have been removed.

## v0.32.0 - 2024-11-28

- The `gleam/os` environment functions have been deprecated in favour of the
  `envoy` package.

## v0.31.0 - 2024-11-27

- The `gleam/erlang` module gains the `phash2` and
  `bounded_phash2` functions.

## v0.30.0 - 2024-11-11

- The `gleam/erlang/process` module gains the `deselecting` function.

## v0.29.0 - 2024-11-10

- The `gleam/erlang/process` module gains the `call_forever` and
  `try_call_forever` functions.

## v0.28.0 - 2024-10-24

- The `gleam/erlang/process` module gains the `deselecting_process_down`
  function.

## v0.27.0 - 2024-09-30

- Add `reference_from_dynamic` to `gleam/erlang` for decoding references
  [Erlang Reference](https://www.erlang.org/doc/system/data_types#reference)

## v0.26.0 - 2024-08-19

- Add `port_from_dynamic` for decoding
  [Erlang Port](https://www.erlang.org/doc/system/ports).
- Add `gleam/erlang/port` and the `Port` type.

## v0.25.0 - 2024-03-20

- Updated for `gleam_stdlib` v0.33.0.

## v0.24.0 - 2024-01-03

- Updated for `gleam_stdlib` v0.33.0.
- The `gleam/erlang/file` module has been removed.
- The `start_arguments` function has been deprecated in favour of the `argv`
  package.

## v0.23.1 - 2023-11-15

- Fixed some internal deprecation warnings.

## v0.23.0 - 2023-11-02

- The `gleam/erlang/file` module has been deprecated.
- Updated for Gleam v0.32.0.

## v0.22.0 - 2023-08-30

- The `gleam/erlang/process` module gains the `register`, `unregister`, and
  `named` functions.
- The `gleam/erlang/node` module has been created with the `Node` and
  `ConnectError` types, and the `self`, `visible`, `connect`, `send`, and
  `to_atom` functions.

## v0.21.0 - 2023-08-25

- The `gleam/erlang` module gains the `priv_directory` function.

## v0.20.0 - 2023-08-03

- Updated for Gleam v0.30.0.

## v0.19.0 - 2023-05-17

- The `gleam/erlang/process` module gains functions `selecting_record5`
  through to `selecting_record8`.
- The `file` module loses the `is_file` function; gains the `FileType`,
  `Access`, and `FileInfo` types; and gains the `is_regular`, `file_info`,
  `link_info`, `file_exists`, and `link_exists` functions.

## v0.18.1 - 2023-03-02

- Updated for Gleam 0.27.0.

## v0.18.0 - 2023-02-03

- The `process.send_abnormal_exit` function no longer has labels.

## v0.17.1 - 2022-12-08

- Fixed a bug where the `to_string` and `from_string` functions in the
  `charlist` module would handle some unicode characters incorrectly.

## v0.17.0 - 2022-10-20

- The `gleam/file` module gains the `append` and `append_bits` functions.

## v0.16.0 - 2022-10-13

- This library now requires OTP 23.0 or higher.
- `atom.create_from_string` calls the `erlang:binary_to_atom` built-in function
  directly, enabling the compilers to optimise the call to an atom literal when
  the argument is a string literal.
- `atom.to_string` calls the `erlang:atom_to_binary` built-in function directly,
  enabling the compilers to optimise the call to a string literal when the
  argument is an atom literal.

## v0.15.0 - 2022-08-07

- The `gleam/erlang/process` module's `selecting_subjectless_record` function
  has been replaced by the `selecting_record2`, `selecting_record3`, and
  `selecting_record4` functions.
- The `gleam/erlang/process` module gains the `selecting_anything` function.

## v0.14.0 - 2022-08-01

- The `gleam/erlang/process` module gains the `flush_messages` and
  `selecting_trapped_exits` functions.

## v0.13.0 - 2022-07-30

- The `gleam/erlang/process` module gains the `map_selector` and
  `merge_selector` functions.

## v0.12.0 - 2022-07-30

- The `gleam/erlang/process.select_forever` function no longer returns a result.

## v0.11.0 - 2022-07-30

- The `gleam/erlang/process` module gains the `send_after`, `cancel_timer`,
  `send_exit`, `send_abnormal_exit`, `kill`, `trap_exits`, `link`, and `unlink`
  functions.

## v0.10.0 - 2022-07-19

- The `gleam/erlang` module gains the `Reference` type and `make_reference`
  function.
- The `gleam/erlang/process` module has been created with the `Pid`, `Subject`,
  `Selector`, and `CallError` types and the `self`, `start`, `new_subject`,
  `subject_owner`, `receive`, `new_selector`, `select`, `selecting`, `sleep`,
  `sleep_forever`, `monitoring_process`, `selecting_process_down`
  `demonitor_process`, `try_call`, `call`, `selecting_subjectless_record`, and
  `is_alive` functions.
- The `sleep` and `sleep_forever` functions have been moved to the
  `gleam/erlang/process` module.

## v0.9.3 - 2022-05-24

- Fixed some warnings with latest version of Gleam.

## v0.9.2 - 2022-02-07

- Switched from `atom_to_binary/1` to `atom_to_binary/2` to support older
  versions of Erlang.

## v0.9.1 - 2022-02-04

- Corrected the `NotUTF8` file error variant name to `NotUtf8`.

## v0.9.0 - 2022-01-26

- The `os` module gains the `family` function.
- Fixed a bug where the `erlang.sleep` function would fail.

## v0.8.0 - 2022-01-18

- Use `DecodeErrors` in the `atom` module for the `from_dynamic` function.

## v0.7.0 - 2022-01-13

- Add `is_directory`, `is_file`, `make_directory`, `list_directory`,
  `delete_directory`, `recursive_delete` functions to `file` module.
- Add `os` module with `get_all_env`, `get_env`, `set_env`, and `unset_env`
  functions.

## v0.6.0 - 2021-12-29

- Add `file` module featuring `read`, `read_bits`, `write`, `write_bits`, and
  `delete` functions.

## v0.5.0 - 2021-12-04

- The `erlang` module gains the `sleep` and `sleep_forever` functions.

## v0.4.0 - 2021-11-30

- The `erlang` module gains the `ensure_all_started` function.

## v0.3.0 - 2021-11-30

- The `erlang` module gains the `start_arguments` function.

## v0.2.0 - 2021-11-23

- Converted from rebar3 to the Gleam build tool.

## v0.1.0 - 2021-09-11

- Initial release
