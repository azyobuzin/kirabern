module Kirabern.Compiler.IR

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
    | Label of Label
    | Nop
    | Ret of Exp option

and BinOp =
    | Plus | Minus | Mul | Div
    | And | Or | LShift | RShift | ArShift | Xor

and RelOp =
    | Eq | Ne | Lt | Gt | Le | Ge
    | ULt | ULe | UGt | UGe

and Variable =
    | NamedVariable of Level * string * Types.Ty * unit ref
    | EscapedNamedVariable of Level * string * Types.Ty
    | TempVariable of Types.Ty * unit ref
    | ParameterVaribale of Level * int * Types.Ty
    | EscapedParameterVariable of Level * int * Types.Ty

and Level(name: string, returnType: Types.Ty, parent: Level option) =
    let arguments = System.Collections.Generic.List<string * Types.Ty>()
    let mutable needsEscapeClass = false
    member this.Name = name
    member this.Parent = parent
    member this.ReturnType = returnType
    member this.CreateChild (name, returnType) =
        needsEscapeClass <- true
        Level(name, returnType, Some(this))
    member this.CreateVar (name, ty, escape) =
        if escape then
            needsEscapeClass <- true
            EscapedNamedVariable(this, name, ty)
        else
            NamedVariable(this, name, ty, ref ())
    member this.AddArgument (name, ty, escape) =
        let index = arguments.Count
        arguments.Add((name, ty))
        if escape then
            needsEscapeClass <- true
            EscapedParameterVariable(this, index, ty)
        else
            ParameterVaribale(this, index, ty)
    member val Body = [] : Stm list with get, set
    override this.ToString() = sprintf "Level '%s'" name

and Label = unit ref

let newTopLevel () = Level("Main", Types.Void, None)
let dummyLevel = Level("Dummy", Types.Void, None)

let newTemp ty = TempVariable(ty, ref ())

let newLabel () : Label = ref ()

let getVarTy var =
    match var with
    | NamedVariable(_, _, x, _)
    | EscapedNamedVariable(_, _, x)
    | TempVariable(x, _)
    | ParameterVaribale(_, _, x)
    | EscapedParameterVariable(_, _, x) -> x

let rec getExpTy exp =
    match exp with
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
