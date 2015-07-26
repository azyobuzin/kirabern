module Kirabern.Compiler.Env

type VarEntryInfo = { access: Level.VarInLevel; ty: Types.Ty }
type FunEntryInfo = { level: Level.Level; formals: (string * Types.Ty) list; result: Types.Ty }

type Entry =
    | VarEntry of VarEntryInfo
    | FunEntry of FunEntryInfo

let baseTEnv = 
    Map.ofArray [| "int", Types.Int
                   "string", Types.String |]
let baseVEnv = 
    Map.ofArray [| "print", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "value", Types.String ]
                              result = Types.Void }
                   "println", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "value", Types.String ]
                              result = Types.Void }
                   "readLine", 
                   FunEntry { level = Level.dummyLevel
                              formals = []
                              result = Types.String }
                   "parseInt", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "s", Types.String ]
                              result = Types.Int }
                   "intToString", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "i", Types.Int ]
                              result = Types.String }
                   "not", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "value", Types.Int ]
                              result = Types.Int }
                   "exit", 
                   FunEntry { level = Level.dummyLevel
                              formals = [ "exitCode", Types.Int ]
                              result = Types.Void } |]
