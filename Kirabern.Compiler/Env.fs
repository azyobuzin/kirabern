module Kirabern.Compiler.Env
open IR

type VarEntryInfo = { access: Variable; ty: Types.Ty }
type FunEntryInfo = { level: Level; formals: (string * Types.Ty) list; result: Types.Ty }

type Entry =
    | VarEntry of VarEntryInfo
    | FunEntry of FunEntryInfo

let baseTEnv =
    Map.ofArray [| "int", Types.Int
                   "string", Types.String |]

let baseVEnv =
    let entry name methodInfo args result = 
        let l = Level("$" + name, result, None)
        let exps = args |> List.map (fun (name, ty) -> Var(l.AddParameter(name, ty, false)))
        l.Body <- match result with
                  | Types.Void -> 
                      Seq(CallStaticMethodStm(methodInfo, exps), Ret(None))
                  | _ -> Ret(Some(CallStaticMethodExp(methodInfo, exps, result)))
        name, 
        FunEntry { level = l
                   formals = args
                   result = result }
    
    let notEntry = 
        let l = Level("$not", Types.Int, None)
        let var = Var(l.AddParameter("value", Types.Int, false))
        let t, f = newLabel(), newLabel()
        l.Body <- Ret(Some(Ceq(var, LdcI4(0))))
        "not", 
        FunEntry { level = l
                   formals = [ "value", Types.Int ]
                   result = Types.Int }

    let lenEntry = 
        let l = Level("$len", Types.Int, None)
        let arg = Var(l.AddParameter("array", Types.ArrayType, false))
        l.Body <- Ret(Some(ConvI4(Ldlen(arg))))
        "len", 
        FunEntry { level = l
                   formals = [ "array", Types.ArrayType ]
                   result = Types.Int }
    
    Map.ofArray [| entry "print" (typeof<System.Console>.GetMethod("Write", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "println" (typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "readLine" (typeof<System.Console>.GetMethod("ReadLine")) [] Types.String                   
                   entry "parseInt" (typeof<int>.GetMethod("Parse", [| typeof<string> |])) [ "s", Types.String ] Types.Int
                   entry "intToString" (typeof<System.Convert>.GetMethod("ToString", [| typeof<int32> |])) [ "value", Types.Int ] Types.String
                   notEntry
                   lenEntry
                   entry "getArgs" (typeof<System.Environment>.GetMethod("GetCommandLineArgs")) [] (Types.Array(Types.String))
                   entry "exit" (typeof<System.Environment>.GetMethod("Exit")) [ "exitCode", Types.Int ] Types.Void |]
