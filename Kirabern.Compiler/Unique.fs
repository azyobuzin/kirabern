module Kirabern.Compiler.Unique

type UniqueId = int

let mutable private current = 0

let uniqueId () : UniqueId =
    System.Threading.Interlocked.Increment(&current)
