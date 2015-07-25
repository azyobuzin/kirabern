#load "TestUtils.fsx"

open Kirabern.Compiler

try
    let program = TestUtils.parseProgram()
    printfn "%+A" program
    printfn ""
with ex ->
    printfn "Exception"
    printfn "%O" ex
    printfn ""

for x in Parser.errors.Value do
    printfn "Error"
    printfn "%+A" x
    printfn ""
