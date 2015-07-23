module Absyn
open Microsoft.FSharp.Text.Lexing

type Pos = Position * Position
and Symbol = string * Pos
and ExpPos = Exp * Pos
and TyPos = TyId * Pos

and Var =
    | SimpleVar of Symbol
    | FieldVar of (Var * Pos) * Symbol
    | SubscriptVar of (Var * Pos) * ExpPos

and TyId =
    | SimpleTyId of Symbol
    | ArrayTyId of TyPos

and CallInfo = {func: Symbol; args: ExpPos list; pos: Pos}
and OpInfo = {left: ExpPos; oper: Operator; right: ExpPos; pos: Pos}
and InitRecordInfo = {fields: (Symbol * ExpPos) list; typ: Symbol; pos: Pos}
and AssignInfo = {var: Var; exp: Exp; pos: Pos}
and IfInfo = {test: ExpPos; then': Exp; else': Exp option; pos: Pos}
and WhileInfo = {test: ExpPos; body: Exp; pos: Pos}
and ForInfo = {var: string; escape: bool ref; lo: Exp; hi: Exp; body: Exp; pos: Pos}
and InitArrayInfo = {typ: TyId; size: ExpPos; pos: Pos}

and Exp =
    | VarExp of Var
    | NullExp
    | IntExp of int
    | NegateExp of ExpPos
    | StringExp of string
    | CallExp of CallInfo
    | OpExp of OpInfo
    | RecordExp of InitRecordInfo
    | SeqExp of ExpPos list
    | AssignExp of AssignInfo
    | IfExp of IfInfo
    | WhileExp of WhileInfo
    | ForExp of ForInfo
    | BreakExp of Pos
    | ArrayExp of InitArrayInfo
    | DecExp of Dec
    | VoidExp
    | ErrExp

and VarDecInfo = {name: string; escape: bool ref; typ: TyPos option; init: Exp; pos: Pos}
and TypeDecInfo = {name: string; ty: Ty; pos: Pos}
and FunDecInfo = {name: string; params': Field list; result: TyPos option; body: Exp; pos: Pos}

and Dec =
    | FunDec of FunDecInfo list
    | VarDec of VarDecInfo
    | TypeDec of TypeDecInfo list

and Field = {name: string; escape: bool ref; typ: TyPos; pos: Pos}

and Ty =
    | NameTy of TyPos
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

and Program = ExpPos list
