module Kirabern.Compiler.Level

type Variable =
    | NamedVariable of string * Types.Ty
    | EscapedNamedVariable of string * Types.Ty
    | TempVariable of Types.Ty
    | ParameterVaribale of int
    | EscapedParameterVariable of int * Types.Ty

type Level(name: string, parent: Level option) =
    let arguments = System.Collections.Generic.List<string * Types.Ty>()
    let locals = System.Collections.Generic.List<Variable>()
    let children = System.Collections.Generic.List<Level>()
    member this.Name = name
    member this.Parent = parent
    member this.AddChild level = children.Add(level)
    member this.CreateVar name ty escape : VarInLevel =
        let l =
            if escape then EscapedNamedVariable(name, ty)
            else NamedVariable(name, ty)
        locals.Add(l)
        this, l
    member this.AddArgument name ty escape : VarInLevel =
        let index = arguments.Count
        arguments.Add((name, ty))
        let l =
            if escape then EscapedParameterVariable(index, ty)
            else ParameterVaribale(index)
        locals.Add(l)
        this, l

and VarInLevel = Level * Variable

let newTopLevel () = Level("Main", None)
let dummyLevel = Level("Dummy", None)

type Label = unit ref

let newLabel () = ref ()
