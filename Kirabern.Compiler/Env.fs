module Kirabern.Compiler.Env
open System
open IR

type VarEntryInfo = { access: Variable; ty: Types.Ty }

type Entry =
    | VarEntry of VarEntryInfo
    | FunEntry of Level

let baseTEnv =
    Map.ofArray [| "int", Types.Int
                   "string", Types.String |]

let baseVEnv =
    let entry name methodInfo args result = 
        let l = Level("$" + name, result, None)
        let exps = args |> List.map (fun (name, ty) -> Var(l.AddParameter(name, ty, false)))
        l.Body <- match result with
                  | Types.Void -> 
                      Seq(CallCliMethodStm(methodInfo, exps), Ret(None))
                  | _ -> Ret(Some(CallCliMethodExp(methodInfo, exps, result)))
        name, FunEntry(l)

    let strToArrayEntry =
        let l = Level("$strToArray", Types.Array(Types.Int), None)
        let arg = Var(l.AddParameter("s", Types.String, false))
        let len = Var(newTemp(Types.Int))
        let resArray = Var(newTemp(Types.Array(Types.Int)))
        let i = Var(newTemp(Types.Int))
        let loopStart = newLabel()
        let loopEnd = newLabel()
        l.Body <- Translate.seq
            [ Store(len, CallCliMethodExp(typeof<string>.GetMethod("get_Length"), [arg], Types.Int))
              Store(resArray, NewArray(Types.Int, len))
              Store(i, LdcI4(0))
              MarkLabel(loopStart)
              Bge(i, len, loopEnd)
              // stelem.i4 で char -> int 変換を行う
              Store(Ldelem(resArray, i), CallCliMethodExp(typeof<string>.GetMethod("get_Chars"), [ arg; i ], Types.Char))
              Store(i, Add(i, LdcI4(1)))
              Br(loopStart)
              MarkLabel(loopEnd)
              Ret(Some(resArray)) ]
        "strToArray", FunEntry(l)

    let arrayToStrEntry =
        let l = Level("$arrayToStr", Types.String, None)
        let arg = Var(l.AddParameter("array", Types.Array(Types.Int), false))
        let len = Var(newTemp(Types.Int))
        let charArray = Var(newTemp(Types.Array(Types.Char)))
        let i = Var(newTemp(Types.Int))
        let loopStart = newLabel()
        let loopEnd = newLabel()
        l.Body <- Translate.seq
            [ Store(len, Ldlen(arg))
              Store(charArray, NewArray(Types.Char, len))
              Store(i, LdcI4(0))
              MarkLabel(loopStart)
              Bge(i, len, loopEnd)
              // stelem.i2 で int -> char 変換を行う
              Store(Ldelem(charArray, i), Ldelem(arg, i))
              Store(i, Add(i, LdcI4(1)))
              Br(loopStart)
              MarkLabel(loopEnd)
              Ret(Some(Newobj(typeof<string>.GetConstructor([| typeof<char[]> |]), [charArray], Types.String))) ]
        "arrayToStr", FunEntry(l)

    let notEntry = 
        let l = Level("$not", Types.Int, None)
        let var = Var(l.AddParameter("value", Types.Int, false))
        let t, f = newLabel(), newLabel()
        l.Body <- Ret(Some(Ceq(var, LdcI4(0))))
        "not", FunEntry(l)

    let lenEntry = 
        let l = Level("$len", Types.Int, None)
        let arg = Var(l.AddParameter("array", Types.ArrayType, false))
        l.Body <- Ret(Some(ConvI4(Ldlen(arg))))
        "len", FunEntry(l)
    
    Map.ofArray [| entry "print" (typeof<Console>.GetMethod("Write", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "println" (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "read" (typeof<Console>.GetMethod("Read")) [] Types.Int
                   entry "readln" (typeof<Console>.GetMethod("ReadLine")) [] Types.String                   
                   entry "parseInt" (typeof<int>.GetMethod("Parse", [| typeof<string> |])) [ "s", Types.String ] Types.Int
                   entry "intToStr" (typeof<Convert>.GetMethod("ToString", [| typeof<int32> |])) [ "value", Types.Int ] Types.String
                   entry "concat" (typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |])) [ "str0", Types.String; "str1", Types.String ] Types.String
                   entry "strlen" (typeof<string>.GetMethod("get_Length")) [ "s", Types.String ] Types.Int
                   strToArrayEntry
                   arrayToStrEntry
                   notEntry
                   lenEntry
                   entry "getArgs" (typeof<Environment>.GetMethod("GetCommandLineArgs")) [] (Types.Array(Types.String))
                   entry "exit" (typeof<Environment>.GetMethod("Exit")) [ "exitCode", Types.Int ] Types.Void |]
