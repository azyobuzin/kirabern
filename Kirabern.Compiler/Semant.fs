module Semant
open System
open Microsoft.FSharp.Text.Lexing
open Absyn
open Env

type VEnv = Map<string, Env.Entry>
type TEnv = Map<string, Types.Ty>
type ExpTy = { exp: Translate.Exp; ty: Types.Ty }

type ErrorInfo = { position: Position * Position; message: string }
exception SemanticError of ErrorInfo
let private newError (pos, msg) = SemanticError { position = pos; message = msg }

let rec actualTy ty =
    match ty with
    | Types.Alias(_, x) -> actualTy (!x).Value
    | x -> x

let rec transExp ((venv: VEnv, tenv: TEnv) as env) =
    let rec trexp exp =
        match exp with
        | VarExp(var) -> trvar var
        | _ -> raise (NotImplementedException())

    and trvar var =
        match var with
        | SimpleVar(name, pos) ->
            match venv.TryFind(name) with
            | Some(VarEntry x) -> { exp = (); ty = actualTy x.ty }
            | Some(_) -> raise (newError(pos, sprintf "'%s' は変数ではないシンボルです。" name))
            | None -> raise (newError(pos, sprintf "シンボル '%s' が存在しません" name))

        | FieldVar((var, varpos), (field, pos)) ->
            let var = trvar var
            match var.ty with
            | Types.Record(fields, _) as x ->
                match fields |> List.tryFind (fun (name, _) -> name = field) with
                | Some(_, ty) -> { exp = (); ty = actualTy ty }
                | None -> raise (newError(pos, sprintf "'%s' はレコード %s のメンバーではありません。" field (Types.name x)))
            | x -> raise (newError(varpos, sprintf "型 %s はレコードではありません。" (Types.name x)))

        | SubscriptVar((var, varpos), (exp, pos)) ->
            let exp = trexp exp
            match exp.ty with
            | Types.Int -> ()
            | x -> raise (newError(pos, sprintf "添字は int でなければいけませんが実際には %s です。" (Types.name x)))
            let var = trvar var
            match var.ty with
            | Types.Array(ty) -> { exp = (); ty = actualTy ty }
            | x -> raise (newError(varpos, sprintf "型 %s は配列ではありません。" (Types.name x)))

    trexp
