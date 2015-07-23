module Absyn
open Microsoft.FSharp.Text.Lexing

type Pos = Position * Position
type Symbol = string

type Var =
    | SimpleVar of Symbol * Pos
    | FieldVar of (Var * Pos) * (Symbol * Pos)
    | SubscriptVar of (Var * Pos) * (Exp * Pos)

and TyId =
    | SimpleTyId of Symbol * Pos
    | ArrayTyId of TyId

and CallInfo = {func: (Symbol * Pos); args: (Exp * Pos) list; pos: Pos}
and OpInfo = {left: (Exp * Pos); oper: Operator; right: (Exp * Pos); pos: Pos}
and InitRecordInfo = {fields: ((Symbol * Pos) * (Exp * Pos)) list; typ: (Symbol * Pos); pos: Pos}
and AssignInfo = {var: Var; exp: Exp; pos: Pos}
and IfInfo = {test: Exp * Pos; then': Exp; else': Exp option; pos: Pos}
and WhileInfo = {test: Exp; body: Exp; pos: Pos}
and ForInfo = {var: Symbol; escape: bool ref; lo: Exp; hi: Exp; body: Exp; pos: Pos}
and InitArrayInfo = {typ: TyId * Pos; size: Exp * Pos; pos: Pos}

and Exp =
    | VarExp of Var
    | NullExp
    | IntExp of int
    | NegateExp of Exp * Pos
    | StringExp of string
    | CallExp of CallInfo
    | OpExp of OpInfo
    | RecordExp of InitRecordInfo
    | SeqExp of (Exp * Pos) list
    | AssignExp of AssignInfo
    | IfExp of IfInfo
    | WhileExp of WhileInfo
    | ForExp of ForInfo
    | BreakExp of Pos
    | ArrayExp of InitArrayInfo
    | DecExp of Dec
    | VoidExp
    | ErrExp

and VarDecInfo = {name: Symbol; escape: bool ref; typ: (TyId * Pos) option; init: Exp; pos: Pos}
and TypeDecInfo = {name: Symbol; ty: Ty; pos: Pos}
and FunDecInfo = {name: Symbol; params': Field list; result: (TyId * Pos) option; body: Exp; pos: Pos}

and Dec =
    | FunDec of FunDecInfo list
    | VarDec of VarDecInfo
    | TypeDec of TypeDecInfo list

and Field = {name: Symbol; escape: bool ref; typ: TyId; pos: Pos}

and Ty =
    | NameTy of TyId * Pos
    | RecordTy of Field list

and Operator =
    | PlusOp | MinusOp | TimesOp | DivideOp
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
    override x.ToString() =
        match x with
        | PlusOp -> "+"
        | MinusOp -> "-"
        | TimesOp -> "*"
        | DivideOp -> "/"
        | EqOp -> "=="
        | NeqOp -> "!="
        | LtOp -> "<"
        | LeOp -> "<="
        | GtOp -> ">"
        | GeOp -> ">="

and Program = (Exp * Pos) list
