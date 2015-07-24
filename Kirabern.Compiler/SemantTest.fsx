#r "../packages/FsLexYacc.Runtime.6.1.0/lib/net40/FsLexYacc.Runtime.dll"
#load "Absyn.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "Types.fs"
#load "Translate.fs"
#load "Temp.fs"
#load "Env.fs"
#load "Semant.fs"

open System
open System.IO
open Microsoft.FSharp.Text.Lexing
open Semant

let file = Environment.GetCommandLineArgs().[2]
let lexbuf = LexBuffer<_>.FromString(File.ReadAllText(file))

lexbuf.EndPos <- { pos_bol = 0
                   pos_fname = file
                   pos_cnum = 0
                   pos_lnum = 1 }

let program = Parser.start Lexer.token lexbuf

try
    printfn "%+A" (transProg program)
with SemanticError(es) ->
    for e in es do printfn "%+A" e
