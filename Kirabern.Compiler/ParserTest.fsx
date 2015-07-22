#r "../packages/FsLexYacc.Runtime.6.1.0/lib/net40/FsLexYacc.Runtime.dll"
#load "Absyn.fs"
#load "Parser.fs"
#load "Lexer.fs"

open Microsoft.FSharp.Text.Lexing
open System
open System.IO

let file = Environment.GetCommandLineArgs().[2]
let lexbuf = LexBuffer<_>.FromString(File.ReadAllText(file))

lexbuf.EndPos <- { pos_bol = 0
                   pos_fname = file
                   pos_cnum = 0
                   pos_lnum = 1 }
try     
    let program = Parser.start Lexer.token lexbuf
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
