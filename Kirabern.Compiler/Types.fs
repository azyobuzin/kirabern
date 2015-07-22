module Types
open System

type unique = unit ref

let newUnique () = ref ()

type Ty = 
    | Null
    | Int
    | String
    | Record of (string * Ty) list * unique
    | Array of Ty
    | Alias of string * Ty option ref
    | Void

let rec name ty = 
    match ty with
    | Null -> "null"
    | Int -> "int"
    | String -> "string"
    | Record(fields, _) -> 
        sprintf "{ %s }" (String.Join(", ", fields |> Seq.map (fun (n, ty) -> sprintf "%s: %s" n (name ty))))
    | Array(ty) -> sprintf "%s[]" (name ty)
    | Alias(n, _) -> n
    | Void -> "void"
