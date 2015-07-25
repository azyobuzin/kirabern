module FindEscape
open Absyn

type Depth = int
type EscEnv = Map<string, Depth * bool ref>

let rec traverseVar (env: EscEnv) d s =
    match s with
    | SimpleVar(name, _) ->
        match env.TryFind(name) with
        | Some(d', esc) -> if d > d' then esc := true
        | None -> ()
    | FieldVar((var, _), _) | SubscriptVar((var, _), _) -> traverseVar env d var

and traverseExp (env: EscEnv) d =
    let rec trexp s =
        match s with
        | VarExp(var) -> traverseVar env d var
        | NullExp -> ()
        | IntExp(_) -> ()
        | NegateExp(x, _) -> trexp x
        | StringExp(_) -> ()
        | CallExp(x) -> for (exp, _) in x.args do trexp exp
        | OpExp(x) ->
            let (left, _), (right, _) = x.left, x.right
            trexp left
            trexp right
        | RecordExp(x) -> for (_, (exp, _)) in x.fields do trexp exp
        | SeqExp(xs) -> traverseSeqExp env d xs
        | AssignExp(x) ->
            traverseVar env d x.var
            trexp x.exp
        | IfExp(x) ->
            let test, _ = x.test
            trexp test
            trexp x.then'
            match x.else' with
            | Some(y) -> trexp y
            | None -> ()
        | WhileExp(x) ->
            let test, _ = x.test
            trexp test
            trexp x.body
        | ForExp(x) ->
            let (lo, _), (hi, _) = x.lo, x.hi
            trexp lo
            x.escape := false
            let env' = env.Add(x.var, (d, x.escape))
            traverseExp env' d hi
            traverseExp env' d x.body
        | BreakExp(_) -> ()
        | DecExp(x) -> traverseDec env d x |> ignore
        | ArrayExp(x) ->
            let exp, _ = x.size
            trexp exp
        | VoidExp | ErrExp -> ()

    trexp

and traverseDec env d s =
    match s with
    | FunDec(decs) ->
        let d' = d + 1
        for dec in decs do
            let f (tbl: EscEnv) x =
                x.escape := false
                let name, _ = x.name
                tbl.Add(name, (d', x.escape))
            let env' = List.fold f env dec.params'
            let body, _ = dec.body
            traverseExp env' d' body
        env
    | VarDec(x) ->
        x.escape := false
        let exp, _ = x.init
        traverseExp env d exp
        env.Add(x.name, (d, x.escape))
    | TypeDec(_) -> env

and traverseSeqExp env d xs =
    match xs with
    | [] -> ()
    | [exp, _] -> traverseExp env d exp
    | (exp, _) :: ys ->
        let env' =
            match exp with
            | DecExp(x) -> traverseDec env d x
            | _ ->
                traverseExp env d exp
                env
        traverseSeqExp env' d xs

let findEscape (prog: Program) =
    traverseExp Map.empty 0 (SeqExp prog)
