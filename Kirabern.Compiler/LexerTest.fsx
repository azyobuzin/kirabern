#r "../packages/FsLexYacc.Runtime.6.1.0/lib/net40/FsLexYacc.Runtime.dll"
#load "Parser.fs"
#load "Lexer.fs"

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let file = Environment.GetCommandLineArgs().[2]
let lexbuf = LexBuffer<_>.FromString(File.ReadAllText(file))
lexbuf.EndPos <- {
    pos_bol = 0;
    pos_fname = file;
    pos_cnum = 0;
    pos_lnum = 1 }

let rec readToken () =
    let token = Lexer.token lexbuf
    let posToStr (pos: Position) = sprintf "%d:%d" pos.Line pos.Column
    let startPos = posToStr lexbuf.StartPos
    let endPos = posToStr lexbuf.EndPos
    match token with
    | Parser.EOF -> printfn "EOF\t%s, %s" startPos endPos
    | Parser.ID(x) -> printfn "ID(%s)\t%s, %s" x startPos endPos; readToken()
    | Parser.STRING(x) -> printfn "STRING(%s)\t%s, %s" x startPos endPos; readToken()
    | Parser.INT(x) -> printfn "INT(%d)\t%s, %s" x startPos endPos; readToken()
    | x -> printfn "%s\t%s, %s" (Parser.token_to_string x) startPos endPos; readToken()

readToken()
