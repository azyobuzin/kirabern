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
