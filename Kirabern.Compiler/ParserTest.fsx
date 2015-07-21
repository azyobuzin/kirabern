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
    let lexer lexbuf = 
        let token = Lexer.token lexbuf
        let startPos = lexbuf.StartPos
        printfn "%s\t%d:%d" (Parser.token_to_string token) startPos.Line startPos.Column
        token
    
    let program = Parser.start lexer lexbuf
    printfn "%+A" program
with ex -> 
    let startPos = lexbuf.StartPos
    let endPos = lexbuf.EndPos
    printfn "%d:%d, %d:%d" startPos.Line startPos.Column endPos.Line endPos.Column
    printfn "%O" ex
