module Semant
open System
open Microsoft.FSharp.Text.Lexing
open Absyn
open Env

type VEnv = Map<string, Env.Entry>
type TEnv = Map<string, Types.Ty>
type ExpTy = { exp: Translate.Exp; ty: Types.Ty }

type ErrorInfo = { position: Position * Position; message: string }
exception SemanticError of ErrorInfo //TODO: こっちいらなくね？
exception SemanticErrors of ErrorInfo list

let private newError (pos, msg) = SemanticError { position = pos; message = msg }
let private symbolNotExists (pos, name) = newError(pos, sprintf "シンボル '%s' が存在しません" name)
let private loopToCheck<'a> (f: 'a -> unit) (xs: 'a seq) =
    let mutable errors = []
    for x in xs do
        try
            f x
        with
        | SemanticError(e) -> errors <- errors @ [e]
        | SemanticErrors(es) -> errors <- errors @ es
    if errors.Length = 1 then
        raise (SemanticError errors.Head)
    if errors.Length > 1 then
        raise (SemanticErrors errors)

let rec actualTy ty =
    match ty with
    | Types.Alias(_, x) -> actualTy (!x).Value
    | x -> x

let cmpTy x y =
    if (actualTy x) = (actualTy y) then true
    else
        match x with
        | Types.Record(_) -> y = Types.Null
        | Types.Null ->
            match y with
            | Types.Record(_) -> true
            | _ -> false
        | _ -> false

let rec transExp ((venv: VEnv, tenv: TEnv) as env) =
    let rec trexp exp =
        match exp with
        | VarExp(var) -> trvar var
        | NullExp -> { exp = (); ty = Types.Null }
        | IntExp(i) -> { exp = (); ty = Types.Int }
        | NegateExp(exp, pos) ->
            let exp = trexp exp
            match exp.ty with
            | Types.Int -> { exp = (); ty = Types.Int }
            | x -> raise (newError(pos, sprintf "ネゲートは int に対してのみ適用できますが、実際には %O です。" x))
        | StringExp(s) -> { exp = (); ty = Types.String }
        | CallExp(x) ->
            let func, funcpos = x.func
            match venv.TryFind(func) with
            | Some(FunEntry f) ->
                if x.args.Length <> f.formals.Length then
                    raise (newError(x.pos, sprintf "関数 '%s' には %d 個の引数が必要ですが、実際には %d 個指定されています。" func f.formals.Length x.args.Length))

                Seq.zip x.args f.formals
                |> loopToCheck (fun ((arg, argpos), (prmname, formal)) ->
                    let arg = trexp arg
                    if not(cmpTy arg.ty formal) then
                        raise (newError(argpos, sprintf "パラメータ '%s' は型 %O ですが、実際には %O が指定されています。" prmname formal arg.ty))
                )

                { exp = (); ty = actualTy f.result }
            | Some(_) -> raise (newError(funcpos, sprintf "'%s' は関数ではないシンボルです。" func))
            | None -> raise (symbolNotExists(funcpos, func))
        | OpExp(x) ->
            let left, leftpos = x.left
            let left = trexp left
            let right, rightpos = x.right
            let right = trexp right
            match x.oper with
            | PlusOp | MinusOp | TimesOp | DivideOp ->
                let notInt x = not(cmpTy Types.Int x)
                if notInt left.ty then
                    raise (newError(leftpos, sprintf "演算子 %O の左辺は int でなければいけませんが、実際には %O です。" x.oper left.ty))
                if notInt right.ty then
                    raise (newError(rightpos, sprintf "演算子 %O の右辺は int でなければいけませんが、実際には %O です。" x.oper right.ty))
                { exp = (); ty = Types.Int }
            | EqOp | NeqOp | LtOp | LeOp | GeOp | GtOp | GeOp ->
                if not(cmpTy left.ty right.ty) then
                    raise (newError(x.pos, sprintf "演算子 %O の両辺は同じ型でなければいけません。 左辺: %O, 右辺: %O" x.oper left.ty right.ty))
                { exp = (); ty = Types.Int }
        | RecordExp(x) ->
            let typ, typpos = x.typ
            match tenv.TryFind(typ) with
            | Some(y) ->
                match actualTy y with
                | Types.Record(formals, _) as ty ->
                    if x.fields.Length <> formals.Length then
                        raise (newError(x.pos, sprintf "レコード %s のフィールドは %d 個ですが、実際には %d 個指定されています。" typ formals.Length x.fields.Length))

                    let mutable assignedFields = []
                    x.fields |> loopToCheck (fun ((name, namepos), (exp, exppos)) ->
                        if List.contains name assignedFields then
                            raise (newError(namepos, sprintf "フィールド '%s' はすでに代入されています。" name))
                        match formals |> List.tryFind (fun (s, _) -> name = s) with
                        | Some(_, formal) ->
                            assignedFields <- name :: assignedFields
                            let exp = trexp exp
                            if not(cmpTy exp.ty formal) then
                                raise (newError(exppos, sprintf "フィールド %s の型は %O ですが、実際には %O が指定されています。" name formal exp.ty))
                        | None -> raise (newError(namepos, sprintf "'%s' はレコード %s のメンバーではありません。" name typ))
                    )

                    { exp = (); ty = ty }
                | _ -> raise (newError(typpos, sprintf "型 %s はレコードではありません。" typ))
            | None -> raise (symbolNotExists(typpos, typ))
        | SeqExp([]) -> { exp = (); ty = Types.Void }
        | SeqExp(xs) -> transSeqExp env xs            
        | _ -> raise (NotImplementedException())

    and trvar var =
        match var with
        | SimpleVar(name, pos) ->
            match venv.TryFind(name) with
            | Some(VarEntry x) -> { exp = (); ty = actualTy x.ty }
            | Some(_) -> raise (newError(pos, sprintf "'%s' は変数ではないシンボルです。" name))
            | None -> raise (symbolNotExists(pos, name))

        | FieldVar((var, varpos), (field, pos)) ->
            let var = trvar var
            match var.ty with
            | Types.Record(fields, _) as x ->
                match fields |> List.tryFind (fun (name, _) -> name = field) with
                | Some(_, ty) -> { exp = (); ty = actualTy ty }
                | None -> raise (newError(pos, sprintf "'%s' はレコード %O のメンバーではありません。" field x))
            | x -> raise (newError(varpos, sprintf "型 %O はレコードではありません。" x))

        | SubscriptVar((var, varpos), (exp, pos)) ->
            let exp = trexp exp
            if exp.ty <> Types.Int then
                raise (newError(pos, sprintf "添字は int でなければいけませんが、実際には %O です。" exp.ty))
            let var = trvar var
            match var.ty with
            | Types.Array(ty) -> { exp = (); ty = actualTy ty }
            | x -> raise (newError(varpos, sprintf "型 %O は配列ではありません。" x))

    trexp

and transSeqExp env =
    let rec trseqexp xs =
        match xs with
        | [] -> { exp = (); ty = Types.Void }
        | [exp, pos] -> { exp = (); ty = (transExp env exp).ty }
        | (exp, pos) :: ys ->
            transExp env exp |> ignore
            trseqexp ys
    //TODO: エラー収集
    //TODO: DecExp による env 書き換え
    trseqexp
