module Kirabern.Compiler.Level

type Variable =
    | NamedVariable of Level * string * Types.Ty * unit ref
    | EscapedNamedVariable of Level * string * Types.Ty
    | TempVariable of Types.Ty * unit ref
    | ParameterVaribale of Level * int * Types.Ty
    | EscapedParameterVariable of Level * int * Types.Ty

and Level(name: string, returnType: Types.Ty, parent: Level option) =
    let arguments = System.Collections.Generic.List<string * Types.Ty>()
    let children = System.Collections.Generic.List<Level>()
    let mutable needsEscapeClass = false
    member this.Name = name
    member this.Parent = parent
    member this.ReturnType = returnType
    member this.AddChild level =
        needsEscapeClass <- true
        children.Add(level)
    member this.CreateVar name ty escape =
        if escape then
            needsEscapeClass <- true
            EscapedNamedVariable(this, name, ty)
        else
            NamedVariable(this, name, ty, ref ())
    member this.AddArgument name ty escape =
        let index = arguments.Count
        arguments.Add((name, ty))
        if escape then
            needsEscapeClass <- true
            EscapedParameterVariable(this, index, ty)
        else
            ParameterVaribale(this, index, ty)

let newTopLevel () = Level("Main", Types.Void, None)
let dummyLevel = Level("Dummy", Types.Void, None)

let newTemp ty = TempVariable(ty, ref ())

type Label = unit ref

let newLabel () : Label = ref ()
