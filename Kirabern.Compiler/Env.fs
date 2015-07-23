module Env

type VarEntryInfo = { access: Translate.Access; ty: Types.Ty }
type FunEntryInfo = { level: Translate.Level; label: Temp.Label; formals: (string * Types.Ty) list; result: Types.Ty }

type Entry =
    | VarEntry of VarEntryInfo
    | FunEntry of FunEntryInfo
