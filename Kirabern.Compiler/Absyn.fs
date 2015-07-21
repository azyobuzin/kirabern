module Absyn
open Microsoft.FSharp.Text.Lexing

type Pos = Position * Position
type Symbol = string

type Var =
    | SimpleVar of Symbol * Pos
    | FieldVar of Var * Symbol * Pos
    | SubscriptVar of Var * Exp * Pos

and TyId =
    | SimpleTyId of Symbol * Pos
    | ArrayTyId of TyId

and CallInfo = {func: Symbol; args: Exp list; pos: Pos}
and OpInfo = {left: Exp; oper: Operator; right: Exp; pos: Pos}
and InitRecordInfo = {fields: (Symbol * Exp * Pos) list; typ: Symbol; pos: Pos}
and AssignInfo = {var: Var; exp: Exp; pos: Pos}
and IfInfo = {test: Exp * Pos; then': Exp; else': Exp option; pos: Pos}
and WhileInfo = {test: Exp; body: Exp; pos: Pos}
and ForInfo = {var: Symbol; escape: bool ref; lo: Exp; hi: Exp; body: Exp; pos: Pos}
and InitArrayInfo = {typ: TyId * Pos; size: Exp * Pos; pos: Pos}
and Exps = (Exp * Pos) list * (Exp * Pos) option

and Exp =
    | VarExp of Var
    | NullExp
    | IntExp of int
    | NegateExp of Exp * Pos
    | StringExp of string
    | CallExp of CallInfo
    | OpExp of OpInfo
    | RecordExp of InitRecordInfo
    | SeqExp of Exps
    | AssignExp of AssignInfo
    | IfExp of IfInfo
    | WhileExp of WhileInfo
    | ForExp of ForInfo
    | BreakExp of Pos
    | ArrayExp of InitArrayInfo
    | DecExp of Dec
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

and Program = (Exp * Pos) list
