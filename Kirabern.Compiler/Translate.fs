module Kirabern.Compiler.Translate

type Level(name: string, parent: Level option) =
    let locals = System.Collections.Generic.List<Temp.Variable>()
    member x.Name = name
    member x.Parent = parent
    member x.AllocLocal var = locals.Add(var)

let topLevel = Level("Program", None)

type Access = unit
type Exp = unit
