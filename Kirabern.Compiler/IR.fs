module Kirabern.Compiler.IR
open Unique

type Exp =
    | Const of int
    | StringLiteral of string
    | Null
    | BinOpExp of BinOp * Exp * Exp
    | Negate of Exp
    | NewRecord of Types.RecordInfo
    | NewArray of Types.Ty * Exp
    | Var of Variable
    | Field of Exp * Types.RecordInfo * string
    | ArrayElem of Exp * Exp
    | CallExp of Level * Exp list
    | CallStaticMethodExp of System.Reflection.MethodInfo * Exp list * Types.Ty
    | ESeq of Stm * Exp

and Stm =
    | Store of Exp * Exp
    | ExpStm of Exp
    | CallStm of Level * Exp list
    | CallStaticMethodStm of System.Reflection.MethodInfo * Exp list
    | Jump of Label
    | CJump of RelOp * Exp * Exp * Label * Label
    | Seq of Stm * Stm
    | MarkLabel of Label
    | Nop
    | Ret of Exp option

and BinOp =
    | Plus | Minus | Mul | Div
    | And | Or | LShift | RShift | ArShift | Xor

and RelOp =
    | Eq | Ne | Lt | Gt | Le | Ge
    | ULt | ULe | UGt | UGe

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
    member val Body = [] : Stm list with get, set
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
    | Const(_) -> Types.Int
    | StringLiteral(_) -> Types.String
    | Null -> Types.Null
    | BinOpExp(_) -> Types.Int
    | Negate(_) -> Types.Int
    | NewRecord(x) -> Types.Record(x)
    | NewArray(x, _) -> Types.Array(x)
    | Var(x) -> getVarTy x
    | Field(_, record, name) ->
        let _, ty = record.Fields |> List.find (fun (x, _) -> x = name)
        ty
    | ArrayElem(x, _) ->
        match getExpTy x with
        | Types.Array(y) -> y
        | y -> failwith(sprintf "%O is not array" y)
    | CallExp(l, _) -> l.ReturnType
    | CallStaticMethodExp(_, _, x) -> x
    | ESeq(_, x) -> getExpTy x

let notRel =
    function
    | Eq -> Ne
    | Ne -> Eq
    | Lt -> Ge
    | Gt -> Le
    | Le -> Gt
    | Ge -> Lt
    | ULt -> UGe
    | ULe -> UGt
    | UGt -> ULe
    | UGe -> ULt
