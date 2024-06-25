import gleam/dict
import gleam/erlang/os

pub fn get_all_test() {
  os.set_env("MYVAR", "MYVALUE")

  let vars = os.get_all_env()
  let assert Ok("MYVALUE") = dict.get(vars, "MYVAR")

  os.unset_env("MYVAR")
}

pub fn set_get_test() {
  let Nil = os.set_env("MYVAR", "MYVALUE")
  let assert Ok("MYVALUE") = os.get_env("MYVAR")

  let Nil = os.set_env(name: "MYVAR", value: "MYVALUE")
  let assert Ok("MYVALUE") = os.get_env(name: "MYVAR")

  os.unset_env("MYVAR")
}

pub fn unset_test() {
  os.set_env("MYVAR", "MYVALUE")

  let Nil = os.unset_env("MYVAR")
  let assert Error(Nil) = os.get_env("MYVAR")

  os.set_env("MYVAR", "MYVALUE")

  let Nil = os.unset_env(name: "MYVAR")
  let assert Error(Nil) = os.get_env(name: "MYVAR")
}

pub fn get_non_existent_test() {
  // Just to make sure
  os.unset_env("I_DONT_EXIST")

  let assert Error(Nil) = os.get_env("I_DONT_EXIST")
  let Nil = os.unset_env("I_DONT_EXIST")
}
