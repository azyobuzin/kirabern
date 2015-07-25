module Kirabern.Compiler.Temp

type Variable =
    | NamedVariable of string
    | TempVariable

type Label = unit
