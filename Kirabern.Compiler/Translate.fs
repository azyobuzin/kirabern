module Translate
open Kirabern.Compiler
open IR

type Exp =
    | Ex of IR.Exp
    | Nx of IR.Stm

let rec private seq =
    function
    | [] -> invalidArg "stms" "empty"
    | [x] -> x
    | x :: xs -> Seq(x, seq xs)

let unEx =
    function
    | Ex(x) -> x
    | Nx(_) -> failwith "Nx -> Ex"

let rec private expToStm =
    function
    | LdcI4(_) | Ldstr(_) | Ldnull | NewRecord(_) | Var(_) -> Nop
    | Neg(x) | ConvI4(x) | NewArray(_, x) | Field(x, _, _) | Ldlen(x) -> expToStm x
    | Ceq(x, y) | Cgt(x, y) | Clt(x, y) | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | Ldelem(x, y) ->
        Seq(expToStm x, expToStm y)
    | CallExp(_) | CallStaticMethodExp(_) as x -> Pop(x)
    | IfExp(x) ->
        let endLabel = newLabel()
        seq [ x.test
              expToStm x.firstExp
              Br(endLabel)
              MarkLabel(x.label)
              expToStm x.secondExp
              MarkLabel(endLabel) ]
    | ESeq(x, y) -> Seq(x, expToStm y)

let unNx =
    function
    | Ex(x) -> expToStm x
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

type private BranchResult =
    | JumpToThen of Stm
    | JumpToElse of Stm
    | ThenOnly
    | ElseOnly

let private revBranch =
    function
    | JumpToThen(x) -> JumpToElse(x)
    | JumpToElse(x) -> JumpToThen(x)
    | ThenOnly -> ElseOnly
    | ElseOnly -> ThenOnly

let rec private branch exp label =
    match exp with
    | Ceq(x, LdcI4(0)) | Ceq(LdcI4(0), x) -> revBranch(branch x label)
    | Ceq(x, y) -> JumpToThen(Beq(x, y, label))
    | Cgt(x, y) -> JumpToThen(Bgt(x, y, label))
    | Clt(x, y) -> JumpToThen(Blt(x, y, label))
    | Ldnull | LdcI4(0) -> ElseOnly
    | LdcI4(_) -> ThenOnly
    | CallExp(func, [x]) when func.Name = "$not" -> revBranch(branch x label)
    | _ -> JumpToThen(Brtrue(exp, label))

let rec private brfalse exp label =
    let revBrOp =
        function
        | Some(s) ->
            match s with
            | Beq(x, y, l) -> Some(BneUn(x, y, l))
            | Blt(x, y, l) -> Some(Bge(x, y, l))
            | Bgt(x, y, l) -> Some(Ble(x, y, l))
            | Ble(x, y, l) -> Some(Bgt(x, y, l))
            | Bge(x, y, l) -> Some(Blt(x, y, l))
            | BneUn(x, y, l) -> Some(Beq(x, y, l))
            | Brtrue(x, l) -> Some(Brfalse(x, l))
            | Brfalse(x, l) -> Some(Brtrue(x, l))
            | Br(_) -> None
            | _ -> failwith "unreachable"
        | None -> Some(Br(label))

    match exp with
    | Ceq(x, LdcI4(0)) | Ceq(LdcI4(0), x) -> revBrOp(brfalse x label)
    | Ceq(x, y) -> Some(BneUn(x, y, label))
    | Cgt(x, y) -> Some(Ble(x, y, label))
    | Clt(x, y) -> Some(Bge(x, y, label))
    | Ldnull | LdcI4(0) -> Some(Br(label))
    | LdcI4(_) -> None
    | CallExp(func, [x]) when func.Name = "$not" -> revBrOp(brfalse x label)
    | _ -> Some(Brfalse(exp, label))
    
let ifThen test then' =
    let falseLabel = newLabel()
    match brfalse (unEx test) falseLabel with
    | Some(Br(_)) -> Nx(Nop)
    | Some(s) ->
        Nx(seq [ s
                 unNx then'
                 MarkLabel(falseLabel) ])
    | None -> then'

let ifElseVoid test then' else' =
    let label, endLabel = newLabel(), newLabel()
    match branch (unEx test) label with
    | JumpToThen(x) ->
        Nx(seq [ x
                 unNx else'
                 Br(endLabel)
                 MarkLabel(label)
                 unNx then'
                 MarkLabel(endLabel) ])
    | JumpToElse(x) ->
        Nx(seq [ x
                 unNx then'
                 Br(endLabel)
                 MarkLabel(label)
                 unNx else'
                 MarkLabel(endLabel) ])
    | ThenOnly -> then'
    | ElseOnly -> else'

let ifElseExp test then' else' =
    let label = newLabel()
    match branch (unEx test) label with
    | JumpToThen(x) ->
        Ex(IfExp { test = x
                   firstExp = unEx else'
                   label = label
                   secondExp = unEx then' })
    | JumpToElse(x) ->
        Ex(IfExp { test = x
                   firstExp = unEx then'
                   label = label
                   secondExp = unEx else' })
    | ThenOnly -> then'
    | ElseOnly -> else'

let whileExp test body breakLabel =
    let start = newLabel()
    match brfalse (unEx test) breakLabel with
    | Some(Br(_)) -> Nx(Nop)
    | Some(s) -> 
        Nx(seq [ MarkLabel(start)
                 s
                 unNx body
                 Br(start)
                 MarkLabel(breakLabel) ])
    | None -> 
        Nx(seq [ MarkLabel(start)
                 unNx body
                 Br(start)
                 MarkLabel(breakLabel) ])

let forExp var lo hi body breakLabel =
    let bodyLabel = newLabel()
    let hiExp, hiInit =
        match hi with
        | Ex(LdcI4(_) as x) | Ex(Var(_) as x) -> x, []
        | _ ->            
            let hiTmp = Var(newTemp Types.Int)
            hiTmp, [Store(hiTmp, unEx hi)]
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
