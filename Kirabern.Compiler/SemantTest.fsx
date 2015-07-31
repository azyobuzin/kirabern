#load "TestUtils.fsx"

open Kirabern.Compiler
open Kirabern.Compiler.Semant

let program = TestUtils.parseProgram()

try
    let topLevel = IR.newTopLevel "Main"
    transProg (Env.baseVEnv, Env.baseTEnv) topLevel program
    printfn "%+A" topLevel.Body
with SemanticError(es) -> 
    for e in es do
        printfn "%+A" e
