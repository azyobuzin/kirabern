module Absyn
open Microsoft.FSharp.Text.Lexing

type private pos = Position
type Symbol = string

type Var =
    | SimpleVar of Symbol * pos
    | FieldVar of Var * Symbol * pos
    | SubscriptVar of Var * Exp * pos

and CallInfo = {func: Symbol; args: Exp list; pos: pos}
and OpInfo = {left: Exp; oper: Operator; right: Exp; pos: pos}
and InitRecordInfo = {fields: (Symbol * Exp * pos) list; typ: Symbol; pos: pos}
and AssignInfo = {var: Var; exp: Exp; pos: pos}
and IfInfo = {test: Exp; then': Exp; else': Exp option; pos: pos}
and WhileInfo = {test: Exp; body: Exp; pos: pos}
and ForInfo = {var: Symbol; escape: bool ref; lo: Exp; hi: Exp; body: Exp; pos: pos}
and InitArrayInfo = {typ: Symbol; size: Exp; pos: pos}
and Exps = (Exp * pos) list * (Exp * pos) option

and Exp =
    | VarExp of Var
    | NullExp
    | IntExp of int
    | StringExp of string
    | CallExp of CallInfo
    | RecordExp of InitRecordInfo
    | SeqExp of Exps
    | AssignExp of AssignInfo
    | IfExp of IfInfo
    | WhileExp of WhileInfo
    | ForExp of ForInfo
    | BreakExp of pos
    | ArrayExp of InitArrayInfo
    | DecExp of Dec

and VarDecInfo = {name: Symbol; escape: bool ref; typ: (Symbol * pos) option; init: Exp; pos: pos}
and TypeDecInfo = {name: Symbol; ty: Ty; pos: pos}
and FunDec = {name: Symbol; params': Field list; result: (Symbol * pos) option; body: Exp; pos: pos}

and Dec =
    | FunctionDec of FunDec list
    | VarDec of VarDecInfo
    | TypeDec of TypeDecInfo list

and Field = {name: Symbol; escape: bool ref; typ: Symbol; pos: pos}

and Ty =
    | NameTy of Symbol * pos
    | RecordTy of Field list
    | ArrayTy of Symbol * pos

and Operator =
    | PlusOp | MinusOp | TimesOp | DivideOp
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

and Program = (Exp * pos) list
