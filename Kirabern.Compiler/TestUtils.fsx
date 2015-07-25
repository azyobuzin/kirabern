#r "../packages/FsLexYacc.Runtime.6.1.0/lib/net40/FsLexYacc.Runtime.dll"
#r "bin/Debug/Kirabern.Compiler.dll"

open Kirabern.Compiler
open Microsoft.FSharp.Text.Lexing
open System
open System.IO

let getArgFile() = Environment.GetCommandLineArgs().[2]

let initLexBuf() = 
    let file = getArgFile()
    let lexbuf = LexBuffer<_>.FromString(File.ReadAllText(file))
    lexbuf.EndPos <- { pos_bol = 0
                       pos_fname = Path.GetFileName(file)
                       pos_cnum = 0
                       pos_lnum = 1 }
    lexbuf

let parseProgram () = 
    let prog = Parser.start Lexer.token (initLexBuf())
    prog
