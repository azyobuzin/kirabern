module Kirabern.Compiler.Tree
open Level

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
    | ESeq of Stm * Exp

and Stm =
    | Store of Exp * Exp
    | ExpStm of Exp
    | CallStm of Level * Exp list
    | Jump of Label
    | CJump of RelOp * Exp * Exp * Label * Label
    | Seq of Stm * Stm
    | Label of Label
    | Nop

and BinOp =
    | Plus | Minus | Mul | Div
    | And | Or | LShift | RShift | ArShift | Xor

and RelOp =
    | Eq | Ne | Lt | Gt | Le | Ge
    | ULt | ULe | UGt | UGe
