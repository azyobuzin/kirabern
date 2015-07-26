#load "TestUtils.fsx"

open Kirabern.Compiler
open Kirabern.Compiler.Semant

let program = TestUtils.parseProgram()

try
    let exp = transProg (Env.baseVEnv, Env.baseTEnv) (Level.newTopLevel()) program
    printfn "%+A" exp
with SemanticError(es) -> 
    for e in es do
        printfn "%+A" e
