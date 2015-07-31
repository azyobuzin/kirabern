open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Text.Lexing
open Kirabern.Compiler

let usage () =
    printfn @"Kirabern Compiler %O

Usage:
kirabern compile sourceFile [outFile]
kirabern run sourceFile"
        (Assembly.GetExecutingAssembly().GetName().Version)

let lexbuf file =
    LexBuffer<char>.FromString(File.ReadAllText(file))

let compile sourceFile outFile =
    let asm =
        Main.compile (lexbuf sourceFile) (Path.GetFileName(sourceFile))
            (Path.GetFileNameWithoutExtension(outFile)) (Path.GetFileName(outFile))
    asm.Save(outFile)

[<EntryPoint>]
let main argv =
    try
        match argv with
        | [| "compile"; sourceFile |] ->
            let outFile = Path.ChangeExtension(Path.GetFileName(sourceFile), ".exe")
            compile sourceFile outFile
            0
        | [| "compile"; sourceFile; outFile |] ->
            compile sourceFile outFile
            0
        | [| "run"; sourceFile |] ->
            let asm = Main.compile (lexbuf sourceFile) (Path.GetFileName(sourceFile)) "Program" "Program"
            let dm = DynamicMethod("Run", typeof<Void>, null, asm.ManifestModule)
            let il = dm.GetILGenerator()
            il.EmitCall(OpCodes.Call, asm.EntryPoint, null)
            il.Emit(OpCodes.Ret)
            (dm.CreateDelegate(typeof<Action>) :?> Action).Invoke()
            0
        | _ ->
            usage()
            1
    with
    | :? Main.ParsingException as ex ->
        let errors =
            ex.Errors |> Seq.map (fun e ->
                let startPos, endPos = e.position
                let stack = String.Join(", ", e.stateStack)
                sprintf "%s\nPos: %d:%d - %d:%d\nStack: %s"
                    e.message startPos.Line startPos.Column endPos.Line endPos.Column stack)
        printfn "%s" (String.Join("\n\n", errors))
        2
    | Semant.SemanticError(es) ->
        let errors = es |> Seq.map (fun e ->
            let startPos, endPos = e.position
            sprintf "%s\nPos: %d:%d - %d:%d"
                e.message startPos.Line startPos.Column endPos.Line endPos.Column)
        printfn "%s" (String.Join("\n\n", errors))
        3
    | ex ->
        printfn "%O" ex
        4
