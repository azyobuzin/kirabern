module Temp

type Variable =
    | NamedVariable of string
    | TempVariable

type Label = Translate.Level * string
