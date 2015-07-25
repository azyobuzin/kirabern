module Kirabern.Compiler.Absyn
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
    | ArrayTyId of TyId

and CallInfo = {func: Symbol; args: ExpPos list; pos: Pos}
and OpInfo = {left: ExpPos; oper: Operator; right: ExpPos; pos: Pos}
and InitRecordInfo = {fields: (Symbol * ExpPos) list; typ: Symbol; pos: Pos}
and AssignInfo = {var: Var; exp: Exp; pos: Pos}
and IfInfo = {test: ExpPos; then': Exp; else': Exp option; pos: Pos}
and WhileInfo = {test: ExpPos; body: Exp; pos: Pos}
and ForInfo = {var: string; escape: bool ref; lo: ExpPos; hi: ExpPos; body: Exp; pos: Pos}
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

and VarDecInfo = {name: string; escape: bool ref; typ: TyId option; init: ExpPos; pos: Pos}
and TypeDecInfo = {name: Symbol; ty: Ty; pos: Pos}
and FunDecInfo = {name: Symbol; params': Field list; result: TyId option; body: ExpPos; pos: Pos}

and Dec =
    | FunDec of FunDecInfo list
    | VarDec of VarDecInfo
    | TypeDec of TypeDecInfo list

and Field = {name: Symbol; escape: bool ref; typ: TyId; pos: Pos}

and Ty =
    | NameTy of TyId
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
