#load "TestUtils.fsx"

open Kirabern.Compiler
open Kirabern.Compiler.Semant

let program = TestUtils.parseProgram()

try 
    printfn "%+A" (transProg program)
with SemanticError(es) -> 
    for e in es do
        printfn "%+A" e
