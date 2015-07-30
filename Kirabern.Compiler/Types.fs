module Kirabern.Compiler.Types
open System

type unique = unit ref

let newUnique () : unique = ref ()

type Ty = 
    | Null
    | Int
    | String
    | Record of RecordInfo
    | Array of Ty
    | Alias of string * Ty option ref
    | Void    
    override x.ToString() =
        match x with
        | Null -> "null"
        | Int -> "int"
        | String -> "string"
        | Record(x) -> 
            sprintf "{ %s }" (String.Join(", ", x.Fields |> Seq.map (fun (n, ty) -> sprintf "%s: %O" n ty)))
        | Array(ty) -> sprintf "%O[]" ty
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
