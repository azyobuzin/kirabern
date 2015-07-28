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

let baseVEnv (topLevel: Level) = 
    let entry name methodInfo args result = 
        let l = topLevel.CreateChild("$" + name, result)
        let exps = args |> List.map (fun (name, ty) -> Var(l.AddArgument(name, ty, false)))
        l.Body <- match result with
                  | Types.Void -> 
                      [ CallStaticMethodStm(methodInfo, exps)
                        Ret(None) ]
                  | _ -> [ Ret(Some(CallStaticMethodExp(methodInfo, exps, result))) ]
        name, 
        FunEntry { level = l
                   formals = args
                   result = result }
    
    let notEntry = 
        let l = topLevel.CreateChild("$not", Types.Int)
        let var = Var(l.AddArgument("value", Types.Int, false))
        let t, f = newLabel(), newLabel()
        l.Body <- [ CJump(Eq, var, Const 0, t, f)
                    Label f
                    Ret(Some(Const 0))
                    Label t
                    Ret(Some(Const 1)) ]
        "not", 
        FunEntry { level = l
                   formals = [ "value", Types.Int ]
                   result = Types.Int }
    
    Map.ofArray [| entry "print" (typeof<System.Console>.GetMethod("Write", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "println" (typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> |])) [ "value", Types.String ] Types.Void
                   entry "readLine" (typeof<System.Console>.GetMethod("ReadLine")) [] Types.String                   
                   entry "parseInt" (typeof<int32>.GetMethod("Parse", [| typeof<string> |])) [ "s", Types.String ] Types.Int
                   entry "intToString" (typeof<System.Convert>.GetMethod("ToString", [| typeof<int32> |])) [ "value", Types.Int ] Types.String
                   notEntry
                   entry "exit" (typeof<System.Environment>.GetMethod("Exit")) [ "exitCode", Types.Int ] Types.Void |]

