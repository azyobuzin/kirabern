module Translate
open Kirabern.Compiler
open IR

type Exp =
    | Ex of IR.Exp
    | Nx of IR.Stm

let rec seq =
    function
    | [] -> invalidArg "stms" "empty"
    | [x] -> x
    | x :: xs -> Seq(x, seq xs)

let unEx =
    function
    | Ex(x) -> x
    | Nx(_) -> failwith "Nx -> Ex"

let unNx =
    function
    | Ex(x) -> Pop(x)
    | Nx(x) -> x

let nullExp = Ex(Ldnull)

let intExp i = Ex(LdcI4(i))

let negateExp exp = Ex(Neg(unEx exp))

let stringExp s = Ex(Ldstr(s))

let callExp (func: Level) args =
    match func.ReturnType with
    | Types.Void -> Nx(CallStm(func, args |> List.map unEx))
    | Types.Null -> failwith "Invalid return type: null"
    | _ -> Ex(CallExp(func, args |> List.map unEx))

let opExp oper left right =
    let left, right = unEx left, unEx right
    let not x = Ceq(x, LdcI4(0))
    Ex(match oper with
       | Absyn.PlusOp -> Add(left, right)
       | Absyn.MinusOp -> Sub(left, right)
       | Absyn.TimesOp -> Mul(left, right)
       | Absyn.DivideOp -> Div(left, right)
       | Absyn.EqOp -> Ceq(left, right)
       | Absyn.NeqOp -> not(Ceq(left, right))
       | Absyn.LtOp -> Clt(left, right)
       | Absyn.LeOp -> not(Cgt(left, right))
       | Absyn.GtOp -> Cgt(left, right)
       | Absyn.GeOp -> not(Clt(left, right)))

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
    let f = newLabel()
    Nx(seq [ Brfalse(unEx test, f)
             unNx then'
             MarkLabel(f) ])

let ifElseVoid test then' else' =
    let t, finish = newLabel(), newLabel()
    Nx(seq [ Brtrue(unEx test, t)
             unNx else'
             Br(finish)
             MarkLabel(t)
             unNx then'
             MarkLabel(finish) ])

let ifElseExp test then' else' =
    let t, f, finish = newLabel(), newLabel(), newLabel()
    Ex(IfExp { test = Brtrue(unEx test, t)
               thenExp = unEx then'
               thenLabel = t
               elseExp = unEx else'
               elseLabel = f
               endLabel = finish })

let whileExp test body breakLabel =
    let start = newLabel()
    Nx(seq [ MarkLabel(start)
             Brfalse(unEx test, breakLabel)
             unNx body
             Br(start)
             MarkLabel(breakLabel) ])

let forExp var lo hi body breakLabel =
    let start, bodyLabel, incLabel = newLabel(), newLabel(), newLabel()
    let hiTmp = newTemp Types.Int
    let hiExp, hiInit =
        match hi with
        | Ex(LdcI4(_) as x) | Ex(Var(_) as x) -> x, []
        | _ -> Var hiTmp, [Store(Var hiTmp, unEx hi)]
    let var = Var(var)
    let header = Store(var, unEx lo) :: hiInit
    Nx(seq (header @ [ Bgt(var, hiExp, breakLabel)
                       MarkLabel(bodyLabel)
                       unNx body
                       Bge(var, hiExp, breakLabel)
                       Store(var, Add(var, LdcI4(1)))
                       Br(bodyLabel)
                       MarkLabel(breakLabel) ]))

let breakExp label = Nx(Br label)

let arrayExp ty size = Ex(NewArray(ty, unEx size))

let voidExp = Nx(Nop)

let simpleVar var = Ex(Var var)

let fieldVar exp record name = Ex(Field(unEx exp, record, name))

let subscriptVar exp idx = Ex(Ldelem(unEx exp, unEx idx))

let seqVoid x xs = Nx(Seq(unNx x, unNx xs))

let seqExp x xs = Ex(ESeq(unNx x, unEx xs))

let funcBody exp ty =
    match ty with
    | Types.Void -> Seq(unNx exp, Ret(None))
    | _ -> Ret(Some(unEx exp))
