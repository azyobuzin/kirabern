module Kirabern.Compiler.Env

type VarEntryInfo = { access: Translate.Access; ty: Types.Ty }
type FunEntryInfo = { level: Translate.Level; label: Temp.Label; formals: (string * Types.Ty) list; result: Types.Ty }

type Entry =
    | VarEntry of VarEntryInfo
    | FunEntry of FunEntryInfo

let baseTEnv = 
    Map.ofArray [| "int", Types.Int
                   "string", Types.String |]
let baseVEnv = 
    Map.ofArray [| "print", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "value", Types.String ]
                              result = Types.Void }
                   "println", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "value", Types.String ]
                              result = Types.Void }
                   "readLine", 
                   FunEntry { level = ()
                              label = ()
                              formals = []
                              result = Types.String }
                   "parseInt", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "s", Types.String ]
                              result = Types.Int }
                   "intToString", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "i", Types.Int ]
                              result = Types.String }
                   "not", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "value", Types.Int ]
                              result = Types.Int }
                   "exit", 
                   FunEntry { level = ()
                              label = ()
                              formals = [ "exitCode", Types.Int ]
                              result = Types.Void } |]
