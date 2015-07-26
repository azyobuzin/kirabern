module Kirabern.Compiler.Tree
open Level

type Exp =
    | ConstInt of int
    | ConstString of string
    | BinOpExp of BinOp
    | Mem of VarInLevel
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

and BinOp =
    | Plus | Minus | Mul | Div
    | And | Or | LShift | RShift | ArShift | Xor

and RelOp =
    | Eq | Ne | Lt | Gt | Le | Ge
    | ULt | ULe | UGt | UGe

