module Kirabern.Compiler.Translate
open Level

type Exp =
    | Ex of Tree.Exp
    | Nx of Tree.Stm
    | Cx of (Label * Label -> Tree.Stm)

let rec seq stms =
    match stms with
    | [] -> invalidArg "stms" "empty"
    | [x] -> x
    | x :: xs -> Tree.Seq(x, seq xs)

let unEx exp =
    match exp with
    | Ex(x) -> x
    | Nx(_) -> failwith "Nx -> Ex"
    | Cx(x) -> 
        let r, t, f = newTemp Types.Int, newLabel(), newLabel()
        Tree.ESeq(seq [ Tree.Store(Tree.Var r, Tree.Const 1)
                        x (t, f)
                        Tree.Label(f)
                        Tree.Store(Tree.Var r, Tree.Const 0)
                        Tree.Label(t) ], Tree.Var r)

let unNx exp =
    match exp with
    | Ex(x) -> Tree.ExpStm(x)
    | Nx(x) -> x
    | Cx(_) -> failwith "Cx -> Nx"

let unCx exp =
    match exp with
    | Ex(Tree.Const 0) -> fun (t, f) -> Tree.Jump(f)
    | Ex(Tree.Const 1) -> fun (t, f) -> Tree.Jump(t)
    | Ex(x) -> fun (t, f) -> Tree.CJump(Tree.Ne, x, Tree.Const 0, t, f)
    | Nx(_) -> failwith "Nx -> Cx"
    | Cx(x) -> x

let nullExp = Ex(Tree.Null)

let intExp i = Ex(Tree.Const(i))

let negateExp exp = Ex(Tree.Negate(unEx exp))

let stringExp s = Ex(Tree.StringLiteral(s))

let callExp (func: Level) args =
    match func.ReturnType with
    | Types.Void -> Nx(Tree.CallStm(func, args |> List.map unEx))
    | Types.Null -> failwith "Invalid return type: null"
    | _ -> Ex(Tree.CallExp(func, args |> List.map unEx))

let opExp oper left right =
    let left, right = unEx left, unEx right
    let bin op = Ex(Tree.BinOpExp(op, left, right))
    let rel op = Cx(fun (t, f) -> Tree.CJump(op, left, right, t, f))
    match oper with
    | Absyn.PlusOp -> bin Tree.Plus
    | Absyn.MinusOp -> bin Tree.Minus
    | Absyn.TimesOp -> bin Tree.Mul
    | Absyn.DivideOp -> bin Tree.Div
    | Absyn.EqOp -> rel Tree.Eq
    | Absyn.NeqOp -> rel Tree.Ne
    | Absyn.LtOp -> rel Tree.Lt
    | Absyn.LeOp -> rel Tree.Le
    | Absyn.GtOp -> rel Tree.Gt
    | Absyn.GeOp -> rel Tree.Ge

let recordExp record fields =
    let tmp = newTemp(Types.Record record)
    let setters =
        fields |> List.map (fun (name, exp) ->
            Tree.Store(Tree.Field(Tree.Var tmp, record, name), unEx exp))
    Ex(Tree.ESeq(
        seq(Tree.Store(Tree.Var tmp, Tree.NewRecord record) :: setters),
        Tree.Var tmp))

let assignExp left right =
    Nx(Tree.Store(unEx left, unEx right))

let ifThen test then' =
    let t, f = newLabel(), newLabel()
    Nx(seq [
        (unCx test)(t, f)
        Tree.Label t
        unNx then'
        Tree.Label f ])

let ifElseVoid test then' else' =
    let t, f, finish = newLabel(), newLabel(), newLabel()
    Nx(seq [
        (unCx test)(t, f)
        Tree.Label t
        unNx then'
        Tree.Jump finish
        Tree.Label f
        unNx else'
        Tree.Label finish ])

let ifElseExp test then' else' ty =
    let tmp = newTemp ty
    let t, f, finish = newLabel(), newLabel(), newLabel()
    Ex(Tree.ESeq(seq [ (unCx test) (t, f)
                       Tree.Label t
                       Tree.Store(Tree.Var tmp, unEx then')
                       Tree.Jump finish
                       Tree.Label f
                       Tree.Store(Tree.Var tmp, unEx else')
                       Tree.Label finish ], Tree.Var tmp))

let whileExp test body breakLabel =
    let start, bodyLabel = newLabel(), newLabel()
    Nx(seq [
        Tree.Label start
        (unCx test)(bodyLabel, breakLabel)
        Tree.Label bodyLabel
        unNx body
        Tree.Jump start
        Tree.Label breakLabel ])

let forExp var lo hi body breakLabel =
    let start, bodyLabel, incLabel = newLabel(), newLabel(), newLabel()
    let hiTmp = newTemp Types.Int
    let hiExp, hiInit =
        match hi with
        | Ex(Tree.Const(i)) -> Tree.Const(i), []
        | _ -> Tree.Var hiTmp, [Tree.Store(Tree.Var hiTmp, unEx hi)]
    let header = Tree.Store(Tree.Var var, unEx lo) :: hiInit
    Nx(seq (header @ [ Tree.Label start
                       Tree.CJump(Tree.Le, Tree.Var var, hiExp, bodyLabel, breakLabel)
                       Tree.Label bodyLabel
                       unNx body
                       Tree.CJump(Tree.Eq, Tree.Var var, hiExp, breakLabel, incLabel)
                       Tree.Label incLabel
                       Tree.Store(Tree.Var var, Tree.BinOpExp(Tree.Plus, Tree.Var var, Tree.Const 1))
                       Tree.Jump start
                       Tree.Label breakLabel ]))

let breakExp label = Nx(Tree.Jump label)

let arrayExp ty size = Ex(Tree.NewArray(ty, unEx size))

let voidExp = Nx(Tree.Nop)

let simpleVar var = Ex(Tree.Var var)

let fieldVar exp record name = Ex(Tree.Field(unEx exp, record, name))

let subscriptVar exp idx = Ex(Tree.ArrayElem(unEx exp, unEx idx))

let seqVoid x xs = Nx(Tree.Seq(unNx x, unNx xs))

let seqExp x xs = Ex(Tree.ESeq(unNx x, unEx xs))
