#load "TestUtils.fsx"

open Kirabern.Compiler
open Microsoft.FSharp.Text.Lexing

let lexbuf = TestUtils.initLexBuf()

let rec readToken() = 
    let token = Lexer.token lexbuf
    let posToStr (pos: Position) = sprintf "%d:%d" pos.Line pos.Column
    let startPos = posToStr lexbuf.StartPos
    let endPos = posToStr lexbuf.EndPos
    match token with
    | Parser.EOF -> printfn "EOF\t%s, %s" startPos endPos
    | Parser.ID(x) -> 
        printfn "ID(%s)\t%s, %s" x startPos endPos
        readToken()
    | Parser.STRING(x) -> 
        printfn "STRING(%s)\t%s, %s" x startPos endPos
        readToken()
    | Parser.INT(x) -> 
        printfn "INT(%d)\t%s, %s" x startPos endPos
        readToken()
    | x -> 
        printfn "%s\t%s, %s" (Parser.token_to_string x) startPos endPos
        readToken()

readToken()
