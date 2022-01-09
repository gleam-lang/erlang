import gleam/erlang/os
import gleam/map

pub fn get_all_test() {
  os.set_env("MYVAR", "MYVALUE")

  let vars = os.get_all_env()
  assert Ok("MYVALUE") = map.get(vars, "MYVAR")

  os.unset_env("MYVAR")
}

pub fn set_get_test() {
  assert Nil = os.set_env("MYVAR", "MYVALUE")
  assert Ok("MYVALUE") = os.get_env("MYVAR")

  assert Nil = os.set_env(name: "MYVAR", value: "MYVALUE")
  assert Ok("MYVALUE") = os.get_env(name: "MYVAR")

  os.unset_env("MYVAR")
}

pub fn unset_test() {
  os.set_env("MYVAR", "MYVALUE")

  assert Nil = os.unset_env("MYVAR")
  assert Error(Nil) = os.get_env("MYVAR")

  os.set_env("MYVAR", "MYVALUE")

  assert Nil = os.unset_env(name: "MYVAR")
  assert Error(Nil) = os.get_env(name: "MYVAR")
}

pub fn get_non_existant_test() {
  // Just to make sure
  os.unset_env("I_DONT_EXIST")

  assert Error(Nil) = os.get_env("I_DONT_EXIST")
  assert Nil = os.unset_env("I_DONT_EXIST")
}
