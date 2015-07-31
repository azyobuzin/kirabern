module Translate
open Kirabern.Compiler
open IR

type Exp =
    | Ex of IR.Exp
    | Nx of IR.Stm
    | Cx of (Label * Label -> IR.Stm)

let rec seq =
    function
    | [] -> invalidArg "stms" "empty"
    | [x] -> x
    | x :: xs -> Seq(x, seq xs)

let unEx =
    function
    | Ex(x) -> x
    | Nx(_) -> failwith "Nx -> Ex"
    | Cx(x) ->
        let r, t, f = newTemp Types.Int, newLabel(), newLabel()
        ESeq(seq [ Store(Var r, Const 1)
                   x (t, f)
                   MarkLabel(f)
                   Store(Var r, Const 0)
                   MarkLabel(t) ], Var r)

let unNx =
    function
    | Ex(x) -> ExpStm(x)
    | Nx(x) -> x
    | Cx(_) -> failwith "Cx -> Nx"

let unCx =
    function
    | Ex(Const 0) -> fun (t, f) -> Jump(f)
    | Ex(Const 1) -> fun (t, f) -> Jump(t)
    | Ex(x) -> fun (t, f) -> CJump(Ne, x, Const 0, t, f)
    | Nx(_) -> failwith "Nx -> Cx"
    | Cx(x) -> x

let nullExp = Ex(Null)

let intExp i = Ex(Const(i))

let negateExp exp = Ex(Negate(unEx exp))

let stringExp s = Ex(StringLiteral(s))

let callExp (func: Level) args =
    match func.ReturnType with
    | Types.Void -> Nx(CallStm(func, args |> List.map unEx))
    | Types.Null -> failwith "Invalid return type: null"
    | _ -> Ex(CallExp(func, args |> List.map unEx))

let opExp oper left right =
    let left, right = unEx left, unEx right
    let bin op = Ex(BinOpExp(op, left, right))
    let rel op = Cx(fun (t, f) -> CJump(op, left, right, t, f))
    match oper with
    | Absyn.PlusOp -> bin Plus
    | Absyn.MinusOp -> bin Minus
    | Absyn.TimesOp -> bin Mul
    | Absyn.DivideOp -> bin Div
    | Absyn.EqOp -> rel Eq
    | Absyn.NeqOp -> rel Ne
    | Absyn.LtOp -> rel Lt
    | Absyn.LeOp -> rel Le
    | Absyn.GtOp -> rel Gt
    | Absyn.GeOp -> rel Ge

let recordExp record fields =
    let tmp = newTemp(Types.Record record)
    let setters =
        fields |> List.map (fun (name, exp) ->
            Store(Field(Var tmp, record, name), unEx exp))
    Ex(ESeq(
        seq(Store(Var tmp, NewRecord record) :: setters),
        Var tmp))

let assignExp left right =
    Nx(Store(unEx left, unEx right))

let ifThen test then' =
    let t, f = newLabel(), newLabel()
    Nx(seq [
        (unCx test)(t, f)
        MarkLabel t
        unNx then'
        MarkLabel f ])

let ifElseVoid test then' else' =
    let t, f, finish = newLabel(), newLabel(), newLabel()
    Nx(seq [
        (unCx test)(t, f)
        MarkLabel t
        unNx then'
        Jump finish
        MarkLabel f
        unNx else'
        MarkLabel finish ])

let ifElseExp test then' else' ty =
    let tmp = newTemp ty
    let t, f, finish = newLabel(), newLabel(), newLabel()
    Ex(ESeq(seq [ (unCx test) (t, f)
                  MarkLabel t
                  Store(Var tmp, unEx then')
                  Jump finish
                  MarkLabel f
                  Store(Var tmp, unEx else')
                  MarkLabel finish ], Var tmp))

let whileExp test body breakLabel =
    let start, bodyLabel = newLabel(), newLabel()
    Nx(seq [
        MarkLabel start
        (unCx test)(bodyLabel, breakLabel)
        MarkLabel bodyLabel
        unNx body
        Jump start
        MarkLabel breakLabel ])

let forExp var lo hi body breakLabel =
    let start, bodyLabel, incLabel = newLabel(), newLabel(), newLabel()
    let hiTmp = newTemp Types.Int
    let hiExp, hiInit =
        match hi with
        | Ex(Const(_) as x) | Ex(Var(_) as x) -> x, []
        | _ -> Var hiTmp, [Store(Var hiTmp, unEx hi)]
    let header = Store(Var var, unEx lo) :: hiInit
    Nx(seq (header @ [ MarkLabel start
                       CJump(Le, Var var, hiExp, bodyLabel, breakLabel)
                       MarkLabel bodyLabel
                       unNx body
                       CJump(Lt, Var var, hiExp, incLabel, breakLabel)
                       MarkLabel incLabel
                       Store(Var var, BinOpExp(Plus, Var var, Const 1))
                       Jump bodyLabel
                       MarkLabel breakLabel ]))

let breakExp label = Nx(Jump label)

let arrayExp ty size = Ex(NewArray(ty, unEx size))

let voidExp = Nx(Nop)

let simpleVar var = Ex(Var var)

let fieldVar exp record name = Ex(Field(unEx exp, record, name))

let subscriptVar exp idx = Ex(ArrayElem(unEx exp, unEx idx))

let seqVoid x xs = Nx(Seq(unNx x, unNx xs))

let seqExp x xs = Ex(ESeq(unNx x, unEx xs))

let funcBody exp ty =
    match ty with
    | Types.Void -> Seq(unNx exp, Ret(None))
    | _ -> Ret(Some(unEx exp))
