module Canon
open Kirabern.Compiler
open IR

#nowarn "25" // Incomplete pattern

let linearize (stm0: Stm) : Stm list =
    let (%) x y =
        match x, y with
        | ExpStm(Const _), z | Nop, z -> z
        | z, ExpStm(Const _) | z, Nop -> z
        | _ -> Seq(x, y)

    let commute t =
        match t with
        | ExpStm(Const _), _ | Nop, _
        | _, Const(_) | _, Null -> true
        | _ -> false

    let rec reorder xs =
        match xs with
        | (CallExp(l, _) as e) :: rest ->
            let t = newTemp l.ReturnType
            reorder(ESeq(Store(Var t, e), Var t) :: rest)
        | a :: rest ->
            let stms, e = doExp a
            let stms', el = reorder rest
            if commute(stms', e) then stms % stms', e :: el
            else
                let t = newTemp(getExpTy e)
                stms % Store(Var t, e) % stms', Var t :: el
        | [] -> Nop, []

    and reorderExp el build =
        let stms, el' = reorder el
        stms, build el'

    and reorderStm el build =
        let stms, el' = reorder el
        stms % build el'

    and doStm x =
        match x with
        | Store(ESeq(s, e), b) -> doStm(Seq(s, Store(e, b)))
        | Store(Var t, CallExp(l, el)) -> reorderStm el (fun el -> Store(Var t, CallExp(l, el))) 
        | Store(a, b) -> reorderStm [a; b] (fun [a; b] -> Store(a, b))
        | ExpStm(e) -> reorderStm [e] (fun [e] -> ExpStm(e))
        | CallStm(l, el) -> reorderStm el (fun el -> CallStm(l, el))
        | CallStaticMethodStm(m, el) -> reorderStm el (fun el -> CallStaticMethodStm(m, el))
        | CJump(p, a, b, t, f) -> reorderStm [a; b] (fun [a; b] -> CJump(p, a, b, t, f))
        | Seq(a, b) -> doStm a % doStm b
        | Ret(Some(e)) -> reorderStm [e] (fun [e] -> Ret(Some(e)))
        | _ -> reorderStm [] (fun _ -> x)

    and doExp x =
        match x with
        | BinOpExp(p, a, b) -> reorderExp [a; b] (fun [a; b] -> BinOpExp(p, a, b))
        | Negate(e) -> reorderExp [e] (fun [e] -> Negate(e))
        | NewArray(t, e) -> reorderExp [e] (fun [e] -> NewArray(t, e))
        | Field(e, r, n) -> reorderExp [e] (fun [e] -> Field(e, r, n))
        | ArrayElem(a, b) -> reorderExp [a; b] (fun [a; b] -> ArrayElem(a, b))
        | CallExp(l, el) -> reorderExp el (fun el -> CallExp(l, el))
        | CallStaticMethodExp(m, el, t) -> reorderExp el (fun el -> CallStaticMethodExp(m, el, t))
        | ESeq(s, e) ->
            let stms = doStm s
            let stms', e = doExp e
            stms % stms', e
        | _ -> reorderExp [] (fun _ -> x)

    and linear x =
        match x with
        | Seq(a, b), l -> linear(a, linear(b, l))
        | s, l -> s :: l

    linear(doStm stm0, [])

let basicBlock stms =
    let finish = newLabel()
    let rec blocks stms blist =
        match stms with
        | (Label(_) as head) :: tail ->
            let rec next xs thisblock =
                match xs with
                | (Jump(_) as s) :: rest -> endblock rest (s :: thisblock)
                | (CJump(_) as s) :: rest -> endblock rest (s :: thisblock)
                | (Label(lab) :: _) as stms -> next (Jump(lab) :: stms) thisblock
                | s :: rest -> next rest (s :: thisblock)
                | [] -> next [Jump(finish)] thisblock

            and endblock stms thisblock = blocks stms (List.rev(thisblock) :: blist)

            next tail [head]
        | [] -> List.rev blist
        | stms -> blocks (Label(newLabel()) :: stms) blist
    blocks stms [], finish

let private enterblock b (table: Map<Label, Stm list>) =
    match b with
    | Label(s) :: _ -> table.Add(s, b)
    | _ -> table

let rec private splitlast xs =
    match xs with
    | [x] -> [], x
    | h :: t ->
        let t', last = splitlast t
        h :: t', last

let rec private trace (table: Map<Label, Stm list>) b rest =
    match b with
    | Label(lab) :: _ ->
        let table = table.Add(lab, [])
        match splitlast b with
        | most, Jump(lab) ->
            match table.TryFind(lab) with
            | Some((_ :: _) as b') -> most @ trace table b' rest
            | _ -> b @ getnext table rest
        | most, CJump(opr, x, y, t, f) ->
            match table.TryFind(t), table.TryFind(f) with
            | _, Some((_ :: _) as b') -> b @ trace table b' rest
            | Some((_ :: _) as b'), _ ->
                most @ [CJump(notRel opr, x, y, f, t)] @ trace table b' rest
            | _ ->
                let f' = newLabel()
                most @ [CJump(opr, x, y, t, f'); Label(f'); Jump(f)] @ getnext table rest
//      | most, Jump(_) -> b @ getnext table rest
        | _ -> failwith "does not end with Jump"
    | _ -> failwith "does not start with Label"

and getnext table b =
    match b with
    | ((Label(lab) :: _) as b) :: rest ->
        match table.TryFind(lab) with
        | Some(_ :: _) -> trace table b rest
        | _ -> getnext table rest
    | [] -> []

let traceSchedule (blocks, finish) =
    getnext (List.foldBack enterblock blocks Map.empty) blocks @ [Label(finish)]
