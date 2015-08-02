module Kirabern.Compiler.Emit
open System
open System.Reflection
open System.Reflection.Emit
open IR

type Universe(moduleBuilder: ModuleBuilder, topClass: TypeBuilder) = 
    let classes = Collections.Generic.List<TypeBuilder>()
    let createdRecords = Collections.Generic.Dictionary<Types.RecordInfo, Type>()
    let parentFieldTable = Collections.Generic.Dictionary<Level, FieldInfo>()
    let escapeClassTable = Collections.Generic.Dictionary<Level, Type>()
    let emitterTable = Collections.Generic.Dictionary<Level, FunctionEmitter>()

    member this.ModuleBuilder = moduleBuilder
    member this.ParentFieldTable = parentFieldTable
    member this.EscapeClassTable = escapeClassTable
    member this.EmitterTable = emitterTable
    
    member this.ReflectionType(ty) = 
        match Types.actualTy ty with
        | Types.Null -> typeof<obj>
        | Types.Int -> typeof<int>
        | Types.String -> typeof<string>
        | Types.Record(x) -> this.GetOrCreateRecordType(x)
        | Types.Array(x) -> this.ReflectionType(x).MakeArrayType()
        | Types.ArrayType -> typeof<Array>
        | Types.Alias(_) -> failwith "unreachable"
        | Types.Void -> typeof<Void>
        
    member this.GetOrCreateRecordType(info: Types.RecordInfo) =
        let mutable ret = null
        if not(createdRecords.TryGetValue(info, &ret)) then
            let t = moduleBuilder.DefineType(info.Name, TypeAttributes.NotPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Class)
            for (name, ty) in info.Fields do
                t.DefineField(name, this.ReflectionType(ty), FieldAttributes.Public) |> ignore
            ret <- t.CreateType()
            createdRecords.Add(info, ret)
        ret

    member this.AddClass(t) = classes.Add(t)

    member this.CreateAllClasses() = 
        for c in classes do
            c.CreateType() |> ignore
        classes.Clear()

    member this.EmitStaticFunction (level: Level) =
        if level.Parent.IsSome then invalidArg "level" "level has a parent"
        let e = FunctionEmitter(this, level, topClass)
        e.Emit()
        e.MethodBuilder
    
and FunctionEmitter(univ: Universe, level: Level, container: TypeBuilder) as this =
    do univ.EmitterTable.Add(level, this)

    let isStatic = level.Parent.IsNone
    let escapeClass, escapeClassCtor, paramVars =
        if level.NeedsEscapeClass then
            let t = univ.ModuleBuilder.DefineType(level.Name, TypeAttributes.NotPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Class)
            univ.AddClass(t)
            univ.EscapeClassTable.Add(level, t)

            let ctor =
                if not isStatic then
                    let parentType = univ.EscapeClassTable.[level.Parent.Value]
                    let fld = t.DefineField("$parent$", parentType, FieldAttributes.Assembly ||| FieldAttributes.InitOnly)
                    univ.ParentFieldTable.Add(level, fld)
                    let ctor = t.DefineConstructor(MethodAttributes.Assembly, CallingConventions.Standard, [| parentType |])
                    let ctorIl = ctor.GetILGenerator()
                    ctorIl.Emit(OpCodes.Ldarg_0)
                    ctorIl.Emit(OpCodes.Callvirt, typeof<obj>.GetConstructor(Type.EmptyTypes))
                    ctorIl.Emit(OpCodes.Ldarg_0)
                    ctorIl.Emit(OpCodes.Ldarg_1)
                    ctorIl.Emit(OpCodes.Stfld, fld)
                    ctorIl.Emit(OpCodes.Ret)
                    ctor
                else t.DefineDefaultConstructor(MethodAttributes.Assembly)

            let paramVars =
                level.Parameters
                |> Seq.mapi (fun i (_, ty, escape) ->
                    i, (if escape then
                            Some(t.DefineField(sprintf "$arg%d$" i, univ.ReflectionType(ty), FieldAttributes.Assembly))
                        else None))
                |> Seq.choose(fun (i, x) ->
                    match x with
                    | Some(y) -> Some(i, y)
                    | None -> None)
                |> Map.ofSeq

            Some(t), Some(ctor), paramVars
        else
            None, None, Map.empty

    
    let escapedVarTable = Collections.Generic.Dictionary<string, FieldBuilder>()

    let methodBuilder =
        let baseMethodAttr = MethodAttributes.Assembly        
        let methodAttr = 
            if isStatic then baseMethodAttr ||| MethodAttributes.Static
            else baseMethodAttr ||| MethodAttributes.Final ||| MethodAttributes.Virtual
        let m =
            container.DefineMethod(level.Name, methodAttr, univ.ReflectionType(level.ReturnType), 
                level.Parameters
                |> Seq.map (fun (_, ty, _) -> univ.ReflectionType(ty))
                |> Seq.toArray)
        level.Parameters |> Seq.iteri (fun i (name, _, _) ->
            m.DefineParameter((if isStatic then i else i + 1), ParameterAttributes.None, name) |> ignore)
        m

    member this.MethodBuilder = methodBuilder

    member this.GetVariableField var =
        match var with
        | EscapedNamedVariable(_, name, ty) ->
            let mutable ret = null
            if not(escapedVarTable.TryGetValue(name, &ret)) then
                ret <- escapeClass.Value.DefineField(name, univ.ReflectionType(ty), FieldAttributes.Assembly)
                escapedVarTable.Add(name, ret)
            ret
        | EscapedParameterVariable(_, i, _) -> paramVars.[i]
        | _ -> invalidArg "var" "not an escaped var"

    member this.Emit() =
        let il = methodBuilder.GetILGenerator()
        let localTable = Collections.Generic.Dictionary<Variable, LocalBuilder>()
        let labelTable = Collections.Generic.Dictionary<Label, Emit.Label>()
                
        let getLocal var =
            let mutable ret = null
            if not(localTable.TryGetValue(var, &ret)) then
                ret <- il.DeclareLocal(univ.ReflectionType(getVarTy var))
                match var with
                | NamedVariable(_, name, _, _) -> ret.SetLocalSymInfo(name)
                | TempVariable(_) -> ()
                | _ -> invalidArg "var" "not a local"
                localTable.Add(var, ret)
            ret

        let getLabel l =
            let mutable ret = Unchecked.defaultof<Emit.Label>
            if not(labelTable.TryGetValue(l, &ret)) then
                ret <- il.DefineLabel()
                labelTable.Add(l, ret)
            ret

        let ldarg idx =
            match if isStatic then idx else idx + 1 with
            | 0 -> il.Emit(OpCodes.Ldarg_0)
            | 1 -> il.Emit(OpCodes.Ldarg_1)
            | 2 -> il.Emit(OpCodes.Ldarg_2)
            | 3 -> il.Emit(OpCodes.Ldarg_3)
            | x when x <= 255 -> il.Emit(OpCodes.Ldarg_S, byte x)
            | x -> il.Emit(OpCodes.Ldarg, int16 x)

        let escapeClassLocal =
            match escapeClass, escapeClassCtor with
            | Some(t), Some(ctor) ->
                let l = il.DeclareLocal(t)
                if not isStatic then
                    il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Newobj, ctor)
                level.Parameters
                |> Seq.mapi (fun i (_, _, escape) -> i, escape)
                |> Seq.choose(function
                              | (i, true) -> Some(i)
                              | _ -> None)
                |> Seq.iter (fun i ->
                    il.Emit(OpCodes.Dup)
                    ldarg i
                    il.Emit(OpCodes.Stfld, paramVars.[i]))
                il.Emit(OpCodes.Stloc, l)
                        
                Some(l)
            | _ -> None

        let emitParent (target: Level) =
            if target = level then
                il.Emit(OpCodes.Ldloc, escapeClassLocal.Value)
            else
                il.Emit(OpCodes.Ldarg_0)
                if level.Parent.Value <> target then
                    let rec f x =
                        il.Emit(OpCodes.Ldfld, univ.ParentFieldTable.[x])
                        let p = x.Parent.Value
                        if p <> target then f p
                    f level.Parent.Value

        let varField var =            
            match var with
            | EscapedNamedVariable(l, _, _) | EscapedParameterVariable(l, _, _) ->
                univ.EmitterTable.[l].GetVariableField(var)
            | _ -> invalidArg "var" "not an escaped var"

        let methodInfo (l: Level) =
            let mutable emitter = Unchecked.defaultof<FunctionEmitter>
            if not(univ.EmitterTable.TryGetValue(l, &emitter)) then
                if l.Parent.IsSome then
                    emitter <- FunctionEmitter(univ, l, escapeClass.Value)
                    emitter.Emit()
                    emitter.MethodBuilder
                else
                    univ.EmitStaticFunction(l)
            else
                emitter.MethodBuilder

        let rec emitExp =
            function
            | LdcI4(x) ->
                match x with
                | 0 -> il.Emit(OpCodes.Ldc_I4_0)
                | 1 -> il.Emit(OpCodes.Ldc_I4_1)
                | 2 -> il.Emit(OpCodes.Ldc_I4_2)
                | 3 -> il.Emit(OpCodes.Ldc_I4_3)
                | 4 -> il.Emit(OpCodes.Ldc_I4_4)
                | 5 -> il.Emit(OpCodes.Ldc_I4_5)
                | 6 -> il.Emit(OpCodes.Ldc_I4_6)
                | 7 -> il.Emit(OpCodes.Ldc_I4_7)
                | 8 -> il.Emit(OpCodes.Ldc_I4_8)
                | -1 -> il.Emit(OpCodes.Ldc_I4_M1)
                | _ when x >= -128 && x <= 127 -> il.Emit(OpCodes.Ldc_I4_S, sbyte x)
                | _ -> il.Emit(OpCodes.Ldc_I4, x)
            | Ldstr(x) -> il.Emit(OpCodes.Ldstr, x)
            | Ldnull -> il.Emit(OpCodes.Ldnull)
            | Ceq(x, y) -> binOp OpCodes.Ceq x y
            | Cgt(x, y) -> binOp OpCodes.Cgt x y
            | Clt(x, y) -> binOp OpCodes.Clt x y
            | Add(x, y) -> binOp OpCodes.Add x y
            | Sub(x, y) -> binOp OpCodes.Sub x y
            | Mul(x, y) -> binOp OpCodes.Mul x y
            | Div(x, y) -> binOp OpCodes.Div x y
            | Neg(x) ->
                emitExp x
                il.Emit(OpCodes.Neg)
            | ConvI4(x) ->
                emitExp x
                il.Emit(OpCodes.Conv_I4)
            | NewRecord(x) -> il.Emit(OpCodes.Newobj, univ.GetOrCreateRecordType(x).GetConstructor(Type.EmptyTypes))
            | NewArray(ty, size) ->
                emitExp size
                il.Emit(OpCodes.Newarr, univ.ReflectionType(ty))
            | Var(x) ->
                match x with
                | NamedVariable(_) | TempVariable(_) -> il.Emit(OpCodes.Ldloc, getLocal x)
                | ParameterVaribale(_, i, _) -> ldarg i
                | EscapedNamedVariable(l, _, _) | EscapedParameterVariable(l, _, _) ->
                    emitParent l
                    il.Emit(OpCodes.Ldfld, varField x)
            | Field(x, record, field) ->
                emitExp x
                il.Emit(OpCodes.Ldfld, univ.GetOrCreateRecordType(record).GetField(field))
            | Ldelem(arr, idx) ->
                emitExp arr
                emitExp idx
                il.Emit(
                    match Types.actualTy(getExpTy arr) with
                    | Types.Array(Types.Int) -> OpCodes.Ldelem_I4
                    | Types.Array(_) -> OpCodes.Ldelem_Ref
                    | _ -> failwith "ldelem: not an array")
            | Ldlen(x) ->
                emitExp x
                il.Emit(OpCodes.Ldlen)
            | CallExp(l, args) -> callFunc l args
            | CallStaticMethodExp(m, args, _) -> callStaticMethod m args
            | IfExp(x) ->
                let endLabel = il.DefineLabel()
                emitStm x.test
                emitExp x.firstExp
                il.Emit(OpCodes.Br, endLabel)
                emitStm(MarkLabel(x.label))
                emitExp x.secondExp
                il.MarkLabel(endLabel)
            | ESeq(x, y) ->
                emitStm x
                emitExp y

        and binOp op left right =
            emitExp left
            emitExp right
            il.Emit(op)

        and emitStm =
            function
            | Store(left, right) ->
                match left with
                | Var(x) ->
                    match x with
                    | NamedVariable(_) | TempVariable(_) ->
                        emitExp right
                        il.Emit(OpCodes.Stloc, getLocal x)
                    | ParameterVaribale(_, i, _) ->
                        emitExp right
                        if i <= 255 then il.Emit(OpCodes.Starg_S, byte i)
                        else il.Emit(OpCodes.Starg, int16 i)
                    | EscapedNamedVariable(l, _, _) | EscapedParameterVariable(l, _, _) ->
                        emitParent l
                        emitExp right
                        il.Emit(OpCodes.Stfld, varField x)
                | Field(x, record, field) ->
                    emitExp x
                    emitExp right
                    il.Emit(OpCodes.Stfld, univ.GetOrCreateRecordType(record).GetField(field))
                | Ldelem(arr, idx) ->
                    emitExp arr
                    emitExp idx
                    emitExp right
                    il.Emit(
                        match Types.actualTy(getExpTy arr) with
                        | Types.Array(Types.Int) -> OpCodes.Stelem_I4
                        | Types.Array(_) -> OpCodes.Stelem_Ref
                        | _ -> failwith "stelem: not an array")
                | _ -> failwith "invalid left"
            | Pop(x) ->
                emitExp x
                il.Emit(OpCodes.Pop)
            | CallStm(l, args) -> callFunc l args
            | CallStaticMethodStm(m, args) -> callStaticMethod m args
            | Br(x) -> il.Emit(OpCodes.Br, getLabel x)
            | Beq(x, y, l) -> brOp OpCodes.Beq x y l
            | Blt(x, y, l) -> brOp OpCodes.Blt x y l
            | Bgt(x, y, l) -> brOp OpCodes.Bgt x y l
            | Ble(x, y, l) -> brOp OpCodes.Ble x y l
            | Bge(x, y, l) -> brOp OpCodes.Bge x y l
            | BneUn(x, y, l) -> brOp OpCodes.Bne_Un x y l
            | Brtrue(x, l) ->
                emitExp x
                il.Emit(OpCodes.Brtrue, getLabel l)
            | Brfalse(x, l) ->
                emitExp x
                il.Emit(OpCodes.Brfalse, getLabel l)
            | Seq(x, y) ->
                emitStm x
                emitStm y
            | MarkLabel(x) ->
                il.MarkLabel(getLabel x)
            | Nop -> ()
            | Ret(x) ->
                match x with | Some(y) -> emitExp y | None -> ()
                il.Emit(OpCodes.Ret)

        and brOp op left right label =
            emitExp left
            emitExp right
            il.Emit(op, getLabel label)

        and callFunc l args =
            match l.Parent with
            | Some(x) -> emitParent x
            | None -> ()
            List.iter emitExp args
            il.EmitCall((if l.Parent.IsSome then OpCodes.Callvirt else OpCodes.Call), methodInfo l, null)

        and callStaticMethod m args =
            List.iter emitExp args
            il.EmitCall(OpCodes.Call, m, null)
                
        emitStm level.Body
