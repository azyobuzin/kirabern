﻿module Kirabern.Compiler.Types
open System

type Ty = 
    | Null
    | Int
    | Char
    | String
    | Record of RecordInfo
    | Array of Ty
    | ArrayType
    | Alias of string * Ty option ref
    | Void    
    override x.ToString() =
        match x with
        | Null -> "null"
        | Int -> "int"
        | Char -> "char"
        | String -> "string"
        | Record(x) -> 
            sprintf "{ %s }" (String.Join(", ", x.Fields |> Seq.map (fun (n, ty) -> sprintf "%s: %O" n ty)))
        | Array(ty) -> sprintf "%O[]" ty
        | ArrayType -> "Array"
        | Alias(n, _) -> n
        | Void -> "void"

and RecordInfo(name: string, fields: (string * Ty) list) =
    member this.Name = name
    member this.Fields = fields

let rec actualTy =
    function 
    | Alias(_, x) -> actualTy (!x).Value
    | Array(x) -> Array(actualTy x)
    | x -> x
