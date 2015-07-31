module Kirabern.Compiler.Main
open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Text.Lexing

type ParsingException(errors: Collections.Generic.IReadOnlyList<Parser.ParseError>, innerException: exn) =
    inherit exn("Parsing error", innerException)
    member this.Errors = errors

let compile (lexbuf: LexBuffer<char>) fileName assemblyName moduleName =
    lexbuf.EndPos <- Position.FirstLine(fileName)

    Parser.clearErrors()
    let prog =
        try
            try
                let result = Parser.start Lexer.token lexbuf
                if Parser.errors.Value.Count > 0 then
                    failwith "パーサーは最後まで走り切りましたがエラーがありました。"
                result
            with
            | ex -> raise (ParsingException(Parser.errors.Value.ToArray(), ex))
        finally
            Parser.clearErrors()

    FindEscape.findEscape prog

    let main = IR.newTopLevel "Main"
    Semant.transProg (Env.baseVEnv, Env.baseTEnv) main prog

    let asm = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.RunAndSave)
    let mdl = asm.DefineDynamicModule(moduleName, true)

    let programClass = mdl.DefineType("Program", TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.NotPublic ||| TypeAttributes.Class)
    let univ = Emit.Universe(mdl, programClass)
    let mainMethod = univ.EmitStaticFunction main
    univ.CreateAllClasses()
    programClass.CreateType() |> ignore

    asm.SetEntryPoint(mainMethod, PEFileKinds.ConsoleApplication)
    asm
