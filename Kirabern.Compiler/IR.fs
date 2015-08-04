module Kirabern.Compiler.IR
open Unique

type Exp =
    | LdcI4 of int
    | Ldstr of string
    | Ldnull
    | Ceq of Exp * Exp
    | Cgt of Exp * Exp
    | Clt of Exp * Exp
    | Add of Exp * Exp
    | Sub of Exp * Exp
    | Mul of Exp * Exp
    | Div of Exp * Exp
    | Neg of Exp
    | ConvI4 of Exp
    | Newobj of System.Reflection.ConstructorInfo * Exp list * Types.Ty
    | NewRecord of Types.RecordInfo
    | NewArray of Types.Ty * Exp
    | Var of Variable
    | Field of Exp * Types.RecordInfo * string
    | Ldelem of Exp * Exp
    | Ldlen of Exp
    | CallExp of Level * Exp list
    | CallCliMethodExp of System.Reflection.MethodInfo * Exp list * Types.Ty
    | IfExp of IfExpInfo
    | ESeq of Stm * Exp

and Stm =
    | Store of Exp * Exp
    | Pop of Exp
    | CallStm of Level * Exp list
    | CallCliMethodStm of System.Reflection.MethodInfo * Exp list
    | Br of Label
    | Beq of Exp * Exp * Label
    | Blt of Exp * Exp * Label
    | Bgt of Exp * Exp * Label
    | Ble of Exp * Exp * Label
    | Bge of Exp * Exp * Label
    | BneUn of Exp * Exp * Label
    | Brtrue of Exp * Label
    | Brfalse of Exp * Label
    | Seq of Stm * Stm
    | MarkLabel of Label
    | Nop
    | Ret of Exp option

and IfExpInfo = { test: Stm; firstExp: Exp; label: Label; secondExp: Exp; }

and Variable =
    | NamedVariable of Level * string * Types.Ty * UniqueId
    | EscapedNamedVariable of Level * string * Types.Ty
    | TempVariable of Types.Ty * UniqueId
    | ParameterVaribale of Level * int * Types.Ty
    | EscapedParameterVariable of Level * int * Types.Ty

and Level(name: string, returnType: Types.Ty, parent: Level option) =
    let parameters = System.Collections.Generic.List<string * Types.Ty * bool>()
    let mutable needsEscapeClass = false
    let mutable varNum = 0
    member this.Name = name
    member this.Parent = parent
    member this.ReturnType = returnType
    member this.Parameters = parameters :> System.Collections.Generic.IReadOnlyList<string * Types.Ty * bool>
    member this.NeedsEscapeClass = needsEscapeClass
    member this.CreateChild (name, returnType) =
        needsEscapeClass <- true
        Level(name, returnType, Some(this))
    member this.CreateVar (name, ty, escape) =
        if escape then
            let n = varNum
            varNum <- n + 1
            EscapedNamedVariable(this, sprintf "%s@%d" name n, ty)
        else
            NamedVariable(this, name, ty, uniqueId())
    member this.AddParameter (name, ty, escape) =
        let index = parameters.Count
        parameters.Add((name, ty, escape))
        if escape then
            EscapedParameterVariable(this, index, ty)
        else
            ParameterVaribale(this, index, ty)
    member val Body = Nop : Stm with get, set
    override this.ToString() = sprintf "Level '%s'" name

and Label = UniqueId

let newTopLevel name = Level(name, Types.Void, None)

let newTemp ty = TempVariable(ty, uniqueId())

let newLabel () : Label = uniqueId()

let getVarTy =
    function
    | NamedVariable(_, _, x, _)
    | EscapedNamedVariable(_, _, x)
    | TempVariable(x, _)
    | ParameterVaribale(_, _, x)
    | EscapedParameterVariable(_, _, x) -> x

let rec getExpTy =
    function
    | LdcI4(_) -> Types.Int
    | Ldstr(_) -> Types.String
    | Ldnull -> Types.Null
    | Ceq(_) | Cgt(_) | Clt(_)
    | Add(_) | Sub(_) | Mul(_) | Div(_)
    | Neg(_) | ConvI4(_) -> Types.Int
    | Newobj(_, _, x) -> x
    | NewRecord(x) -> Types.Record(x)
    | NewArray(x, _) -> Types.Array(x)
    | Var(x) -> getVarTy x
    | Field(_, record, name) ->
        let _, ty = record.Fields |> List.find (fun (x, _) -> x = name)
        ty
    | Ldelem(x, _) ->
        match getExpTy x with
        | Types.Array(y) -> y
        | y -> failwith(sprintf "%O is not array" y)
    | Ldlen(_) -> Types.Int
    | CallExp(l, _) -> l.ReturnType
    | CallCliMethodExp(_, _, x) -> x
    | IfExp(x) -> getExpTy x.firstExp
    | ESeq(_, x) -> getExpTy x
