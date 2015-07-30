module Kirabern.Compiler.Semant
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Text.Lexing
open Absyn
open Env

type VEnv = Map<string, Env.Entry>
type TEnv = Map<string, Types.Ty>
type ExpTy = { exp: Translate.Exp; ty: Types.Ty }

let voidExpTy = { exp = Translate.voidExp; ty = Types.Void }

type ErrorInfo = { position: Position * Position; message: string }
exception SemanticError of ErrorInfo list

let private newError (pos, msg) = SemanticError [ { position = pos; message = msg } ]
let private symbolNotExists (name, pos) = newError(pos, sprintf "シンボル '%s' が存在しません" name)

let private loopToCheck<'a> (f: 'a -> unit) (xs: 'a seq) =
    let mutable errors = []
    for x in xs do
        try
            f x
        with
        | SemanticError(es) -> errors <- errors @ es
    if errors.Length > 0 then
        raise (SemanticError errors)

let private checkedMap<'a, 'b> (f: 'a -> 'b) (xs: 'a seq) =
    let mutable result = []
    let mutable errors = []
    for x in xs do
        try
            result <- result @ [f x]
        with
        | SemanticError(es) -> errors <- errors @ es
    if errors.Length > 0 then
        raise (SemanticError errors)
    result

let actualTy = Types.actualTy

let cmpTy x y =
    let x, y = (actualTy x), (actualTy y)
    if x = y then true
    else
        match x with
        | Types.Record(_) -> y = Types.Null
        | Types.Null ->
            match y with
            | Types.Record(_) -> true
            | _ -> false
        | _ -> false

let isInt = cmpTy Types.Int

let rec getty (tenv: TEnv) tyid =
    match tyid with
    | SimpleTyId(name, pos) ->
        match tenv.TryFind(name) with
        | Some(x) -> x
        | None -> raise (symbolNotExists(name, pos))
    | ArrayTyId(x) -> Types.Array(getty tenv x)

let rec transExp ((venv: VEnv, tenv: TEnv) as env) (level: IR.Level) breakLabel =
    let rec trexp exp =
        match exp with
        | VarExp(var) -> trvar var

        | NullExp -> { exp = Translate.nullExp; ty = Types.Null }

        | IntExp(i) -> { exp = Translate.intExp i; ty = Types.Int }

        | NegateExp(exp, pos) ->
            let exp = trexp exp
            match exp.ty with
            | Types.Int -> { exp = Translate.negateExp exp.exp; ty = Types.Int }
            | x -> raise (newError(pos, sprintf "ネゲートは int に対してのみ適用できますが、指定された型は %O です。" x))

        | StringExp(s) -> { exp = Translate.stringExp s; ty = Types.String }

        | CallExp(x) ->
            let func, funcpos = x.func
            match venv.TryFind(func) with
            | Some(FunEntry f) ->
                if x.args.Length <> f.formals.Length then
                    raise (newError(x.pos, sprintf "関数 '%s' には %d 個の引数が必要ですが、実際には %d 個指定されています。" func f.formals.Length x.args.Length))

                let args =
                    Seq.zip x.args f.formals
                    |> checkedMap (fun ((arg, argpos), (prmname, formal)) ->
                        let arg = trexp arg
                        if not(cmpTy arg.ty formal) then
                            raise (newError(argpos, sprintf "パラメータ '%s' は型 %O ですが、実際には %O が指定されています。" prmname formal arg.ty))
                        arg.exp)

                { exp = Translate.callExp f.level args; ty = actualTy f.result }
            | Some(_) -> raise (newError(funcpos, sprintf "'%s' は関数ではないシンボルです。" func))
            | None -> raise (symbolNotExists(func, funcpos))

        | OpExp(x) ->
            let left, leftpos = x.left
            let left = trexp left
            let right, rightpos = x.right
            let right = trexp right
            match x.oper with
            | PlusOp | MinusOp | TimesOp | DivideOp ->
                let notInt x = not(isInt x)
                if notInt left.ty then
                    raise (newError(leftpos, sprintf "演算子 %O の左辺は int でなければいけませんが、実際には %O です。" x.oper left.ty))
                if notInt right.ty then
                    raise (newError(rightpos, sprintf "演算子 %O の右辺は int でなければいけませんが、実際には %O です。" x.oper right.ty))
            | EqOp | NeqOp | LtOp | LeOp | GeOp | GtOp | GeOp ->
                if not(cmpTy left.ty right.ty) then
                    raise (newError(x.pos, sprintf "演算子 %O の両辺は同じ型でなければいけません。 左辺: %O, 右辺: %O" x.oper left.ty right.ty))
            { exp = Translate.opExp x.oper left.exp right.exp; ty = Types.Int }

        | RecordExp(x) ->
            let typ, typpos = x.typ
            match tenv.TryFind(typ) with
            | Some(y) ->
                match actualTy y with
                | Types.Record(info) as ty ->
                    if x.fields.Length <> info.Fields.Length then
                        raise (newError(x.pos, sprintf "レコード %s のフィールドは %d 個ですが、実際には %d 個指定されています。" typ info.Fields.Length x.fields.Length))

                    let assignedFields = HashSet()
                    let fields =
                        x.fields |> checkedMap (fun ((name, namepos), (exp, exppos)) ->
                            if assignedFields.Contains(name) then
                                raise (newError(namepos, sprintf "フィールド '%s' はすでに代入されています。" name))
                            match info.Fields |> List.tryFind (fun (s, _) -> name = s) with
                            | Some(_, formal) ->
                                assignedFields.Add(name) |> Debug.Assert
                                let exp = trexp exp
                                if not(cmpTy exp.ty formal) then
                                    raise (newError(exppos, sprintf "フィールド %s の型は %O ですが、実際には %O が指定されています。" name formal exp.ty))
                                name, exp.exp
                            | None -> raise (newError(namepos, sprintf "'%s' はレコード %s のメンバーではありません。" name typ)))

                    { exp = Translate.recordExp info fields; ty = ty }
                | _ -> raise (newError(typpos, sprintf "型 %s はレコードではありません。" typ))
            | None -> raise (symbolNotExists(typ, typpos))

        | SeqExp(xs) -> transSeqExp env level breakLabel xs

        | AssignExp(x) ->
            let left = trvar x.var
            let right = trexp x.exp
            if not(cmpTy left.ty right.ty) then
                raise (newError(x.pos, sprintf "左辺の型は %O ですが、右辺の型は %O です。" left.ty right.ty))
            { exp = Translate.assignExp left.exp right.exp; ty = Types.Void }

        | IfExp(x) ->
            let test, testpos = x.test
            let test = trexp test
            if not(isInt test.ty) then
                raise (newError(testpos, sprintf "if の条件は int でなければいけませんが、実際には %O が指定されています。" test.ty))
            let then' = trexp x.then'
            match x.else' with
            | Some(else') ->
                let else' = trexp else'
                if not(cmpTy then'.ty else'.ty) then
                    raise (newError(x.pos, sprintf "then 節と else 節は同じ型でなければいけません。 then: %O, else: %O" then'.ty else'.ty))
                let ty = actualTy then'.ty
                let exp =
                    match ty with
                    | Types.Void -> Translate.ifElseVoid test.exp then'.exp else'.exp
                    | _ -> Translate.ifElseExp test.exp then'.exp else'.exp ty
                { exp = exp; ty = ty }
            | None -> { exp = Translate.ifThen test.exp then'.exp; ty = Types.Void }

        | WhileExp(x) ->
            let test, testpos = x.test
            let test = trexp test
            if not(isInt test.ty) then
                raise (newError(testpos, sprintf "while の条件は int でなければいけませんが、実際には %O が指定されています。" test.ty))
            let breakLabel' = IR.newLabel()
            { exp = Translate.whileExp test.exp (transExp env level (Some breakLabel') x.body).exp breakLabel'; ty = Types.Void }

        | ForExp(x) ->
            let lo, lopos = x.lo
            let lo = trexp lo
            if not(isInt lo.ty) then
                raise (newError(lopos, sprintf "for の初期値は int でなければいけませんが、実際には %O が指定されています。" lo.ty))
            let hi, hipos = x.hi
            let hi = trexp hi
            if not(isInt hi.ty) then
                raise (newError(hipos, sprintf "for の最大値は int でなければいけませんが、実際には %O が指定されています。" hi.ty))
            let var = level.CreateVar(x.var, Types.Int, !x.escape)
            let venv' = venv.Add(x.var, VarEntry { access = var; ty = Types.Int })
            let breakLabel' = IR.newLabel()
            let body = transExp (venv', tenv) level (Some breakLabel') x.body
            { exp = Translate.forExp var lo.exp hi.exp body.exp breakLabel'; ty = Types.Void }

        | BreakExp(pos) ->
            match breakLabel with
            | Some(l) -> { exp = Translate.breakExp l; ty = Types.Void }
            | None -> raise (newError(pos, "この場所で break することはできません。"))

        | ArrayExp(x) ->
            let ty = getty tenv x.typ
            let size, sizepos = x.size
            let size = trexp size
            if not(isInt size.ty) then
                raise (newError(sizepos, sprintf "配列の大きさは int でなければいけませんが、実際には %O が指定されています。" size.ty))
            { exp = Translate.arrayExp ty size.exp; ty = Types.Array(actualTy ty) }

        | DecExp(x) ->
            let exp, _ = transDec env level breakLabel x
            exp

        | VoidExp | ErrExp -> voidExpTy

    and trvar var =
        match var with
        | SimpleVar(name, pos) ->
            match venv.TryFind(name) with
            | Some(VarEntry x) -> { exp = Translate.simpleVar x.access; ty = actualTy x.ty }
            | Some(_) -> raise (newError(pos, sprintf "'%s' は変数ではないシンボルです。" name))
            | None -> raise (symbolNotExists(name, pos))

        | FieldVar((var, varpos), (field, pos)) ->
            let var = trvar var
            match var.ty with
            | Types.Record(info) as x ->
                match info.Fields |> List.tryFind (fun (name, _) -> name = field) with
                | Some(name, ty) -> { exp = Translate.fieldVar var.exp info name; ty = actualTy ty }
                | None -> raise (newError(pos, sprintf "'%s' はレコード %O のメンバーではありません。" field x))
            | x -> raise (newError(varpos, sprintf "型 %O はレコードではありません。" x))

        | SubscriptVar((var, varpos), (exp, pos)) ->
            let exp = trexp exp
            if not(isInt exp.ty) then
                raise (newError(pos, sprintf "添字は int でなければいけませんが、実際には %O です。" exp.ty))
            let var = trvar var
            match var.ty with
            | Types.Array(ty) -> { exp = Translate.subscriptVar var.exp exp.exp; ty = actualTy ty }
            | x -> raise (newError(varpos, sprintf "型 %O は配列ではありません。" x))

    trexp

and transDec ((venv, tenv) as env) level breakLabel dec =
    let getty = getty tenv

    match dec with
    | FunDec(decs) ->
        let funcNames = HashSet()
        let decs = decs |> checkedMap (fun x ->
            let name, namepos = x.name
            if not(funcNames.Add(name)) then
                raise (newError(namepos, sprintf "同名の関数 '%s' を同時に宣言することはできません。" name))
            let startPos, _ = x.pos
            let returnType =
                match x.result with
                | Some(y) -> getty y
                | None -> Types.Void
            x, level.CreateChild(sprintf "%s@%d" name startPos.AbsoluteOffset, returnType)
        )
        let f (tbl: VEnv) (dec, newLevel) =
            let formals =
                dec.params'
                |> List.map (fun x ->
                    let name, _ = x.name
                    name, getty x.typ)
            let name, _ = dec.name
            tbl.Add(name, FunEntry { level = newLevel; formals = formals; result = newLevel.ReturnType })
        let venv' = List.fold f venv decs
        decs |> loopToCheck (fun (x, newLevel) ->
            let prmNames = HashSet()
            x.params' |> loopToCheck (fun x ->
                let name, namepos = x.name
                if not(prmNames.Add(name)) then
                    raise (newError(namepos, sprintf "引数 '%s' はすでに宣言されています。" name))
            )
            let f (tbl: VEnv) prm =
                let name, _ = prm.name
                let ty = getty prm.typ
                tbl.Add(name, VarEntry { access = newLevel.AddParameter(name, ty, !prm.escape); ty = ty })
            let venv'' = List.fold f venv' x.params'
            let body, bodypos = x.body
            let body = transExp (venv'', tenv) newLevel breakLabel body
            match x.result with
            | Some(_) ->
                if not(cmpTy body.ty newLevel.ReturnType) then
                    raise (newError(bodypos, sprintf "戻り値の型は %O と宣言されていますが、実際には %O です。" newLevel.ReturnType body.ty))
            | None -> ()
            newLevel.Body <- Translate.funcBody body.exp newLevel.ReturnType
        )
        voidExpTy, (venv', tenv)

    | VarDec(x) ->
        let init, initpos = x.init
        let init = transExp env level breakLabel init
        let ty =
            match x.typ with
            | Some(y) ->
                let ty = getty y
                if not(cmpTy init.ty ty) then
                    raise (newError(initpos, sprintf "この変数の型は %O と宣言されていますが、右辺は %O です。" ty init.ty))
                ty
            | None ->
                if cmpTy init.ty Types.Null then
                    raise (newError(x.pos, "右辺を null にする場合は方を指定してください。"))
                init.ty
        let var = level.CreateVar(x.name, ty, !x.escape)
        let venv' = venv.Add(x.name, VarEntry { access = var; ty = ty })
        { exp = Translate.assignExp (Translate.simpleVar var) init.exp; ty = Types.Void }, (venv', tenv)

    | TypeDec(decs) ->
        let typeNames = HashSet()
        decs |> loopToCheck (fun x ->
            let name, namepos = x.name
            if not(typeNames.Add(name)) then
                raise (newError(namepos, sprintf "同名の型 '%s' を同時に宣言することはできません。" name))
        )
        let tys = decs |> List.map (fun x -> x, ref None)
        let f (tbl: TEnv) (x: TypeDecInfo, tyref) =
            let name, _ = x.name
            tbl.Add(name, Types.Alias(name, tyref))
        let tenv' = List.fold f tenv tys
        for (x, tyref) in tys do
            tyref := Some(transTy tenv' x)
        voidExpTy, (venv, tenv')

and transTy tenv dec =
    match dec.ty with
    | NameTy(x) -> getty tenv x
    | RecordTy(xs) ->
        let fieldNames = HashSet()
        let fields =
            xs |> checkedMap (fun x ->
                let name, namepos = x.name
                if not(fieldNames.Add(name)) then
                    raise (newError(namepos, sprintf "フィールド '%s' はすでに宣言されています。" name))
                name, getty tenv x.typ)
        let name, _ = dec.name
        let startPos, _ = dec.pos
        Types.Record(Types.RecordInfo(sprintf "%s@%d" name startPos.AbsoluteOffset, fields))

and transSeqExp env level breakLabel xs =
    match xs with
    | [] -> voidExpTy
    | [exp, pos] -> transExp env level breakLabel exp
    | (exp, pos) :: ys ->
        let mutable errors = []
        let exp, env' =
            try
                match exp with
                | DecExp(x) -> transDec env level breakLabel x
                | _ -> transExp env level breakLabel exp, env
            with SemanticError(es) ->
                errors <- es
                voidExpTy, env

        let mutable rest = Unchecked.defaultof<ExpTy>
        try
            rest <- transSeqExp env' level breakLabel ys
        with SemanticError(es) -> errors <- errors @ es

        if errors.Length > 0 then
            raise (SemanticError errors)

        let exp' =
            match rest.ty with
            | Types.Void -> Translate.seqVoid exp.exp rest.exp
            | _ -> Translate.seqExp exp.exp rest.exp
        { exp = exp'; ty = rest.ty }

and transProg env topLevel (prog: Program) =
    let x = transExp env topLevel None (SeqExp prog)
    topLevel.Body <- Translate.funcBody x.exp Types.Void
