module Kirabern.Compiler.Temp

type Variable =
    | NamedVariable of string
    | EscapedNamedVariable of string
    | TempVariable

type Label = unit
