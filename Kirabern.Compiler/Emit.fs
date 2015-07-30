module Kirabern.Compiler.Emit
open System
open System.Reflection
open System.Reflection.Emit
open IR

type Universe(moduleBuilder: ModuleBuilder) = 
    let classes = Collections.Generic.List<TypeBuilder>()
    let createdRecords = Collections.Generic.Dictionary<Types.RecordInfo, Type>()
    let parentFieldTable = Collections.Generic.Dictionary<Level, FieldInfo>()
    let escapeClassTable = Collections.Generic.Dictionary<Level, Type>()
    
    let rec reflectionType ty = 
        match Types.actualTy ty with
        | Types.Null -> typeof<obj>
        | Types.Int -> typeof<int>
        | Types.String -> typeof<string>
        | Types.Record(x) -> getOrCreateRecordType x
        | Types.Array(x) -> (reflectionType x).MakeArrayType()
        | Types.Alias(_) -> failwith "unreachable"
        | Types.Void -> typeof<Void>
        
    and getOrCreateRecordType (info: Types.RecordInfo) =
        let mutable ret = null
        if not(createdRecords.TryGetValue(info, &ret)) then
            let t = moduleBuilder.DefineType(info.Name, TypeAttributes.NotPublic &&& TypeAttributes.Sealed &&& TypeAttributes.Class)
            for (name, ty) in info.Fields do
                t.DefineField(name, reflectionType ty, FieldAttributes.Public) |> ignore
            ret <- t.CreateType()
            createdRecords.Add(info, ret)
        ret
    
    member this.CreateAllClasses() = 
        for c in classes do
            c.CreateType() |> ignore
        classes.Clear()
    
    member this.EmitFunction(l: Level, container: TypeBuilder) = 
        let isStatic = l.Parent.IsNone
        let baseMethodAttr = MethodAttributes.Assembly        
        let methodAttr = 
            if isStatic then baseMethodAttr &&& MethodAttributes.Static
            else baseMethodAttr &&& MethodAttributes.Final        
        let methodBuilder = 
            container.DefineMethod(l.Name, methodAttr, reflectionType l.ReturnType, 
                                   l.Parameters
                                   |> Seq.map (fun (_, ty) -> reflectionType ty)
                                   |> Seq.toArray)        
        l.Parameters |> Seq.iteri (fun i (name, _) ->
            methodBuilder.DefineParameter(i, ParameterAttributes.None, name) |> ignore)

        let il = methodBuilder.GetILGenerator()
        let localTable = Collections.Generic.Dictionary<Variable, LocalBuilder>()
        let labelTable = Collections.Generic.Dictionary<Label, Emit.Label>()

        let escapeData =
            if l.NeedsEscapeClass then
                let t = moduleBuilder.DefineType(l.Name, TypeAttributes.NotPublic &&& TypeAttributes.Sealed &&& TypeAttributes.Class)
                classes.Add(t)
                escapeClassTable.Add(l, t)

                let ctor =
                    if not isStatic then
                        let parentType = escapeClassTable.[l.Parent.Value]
                        let fld = t.DefineField("$parent$", parentType, FieldAttributes.Assembly &&& FieldAttributes.InitOnly)
                        parentFieldTable.Add(l, fld)
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

                if not isStatic then
                    il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Newobj, ctor)
                let v = il.DeclareLocal(t)
                il.Emit(OpCodes.Stloc, v)
                Some(t, v)
            else None

        let getLocal var =
            let mutable ret = null
            if not(localTable.TryGetValue(var, &ret)) then
                ret <- il.DeclareLocal(reflectionType(getVarTy var))
                localTable.Add(var, ret)
            ret

        let getLabel l =
            let mutable ret = Unchecked.defaultof<Emit.Label>
            if not(labelTable.TryGetValue(l, &ret)) then
                ret <- il.DefineLabel()
                labelTable.Add(l, ret)
            ret

        let ldarg i =
            let i = if isStatic then i else i + 1
            match i with
            | 0 -> il.Emit(OpCodes.Ldarg_0)
            | 1 -> il.Emit(OpCodes.Ldarg_1)
            | 2 -> il.Emit(OpCodes.Ldarg_2)
            | 3 -> il.Emit(OpCodes.Ldarg_3)
            | _ when i <= 255 -> il.Emit(OpCodes.Ldarg_S, byte i)
            | _ -> il.Emit(OpCodes.Ldarg, int16 i)

        let rec emitExp =
            function
            | Const(x) ->
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
            | StringLiteral(x) -> il.Emit(OpCodes.Ldstr, x)
            | Null -> il.Emit(OpCodes.Ldnull)
            | BinOpExp(op, left, right) ->
                emitExp left
                emitExp right
                il.Emit(
                    match op with
                    | Plus -> OpCodes.Add
                    | Minus -> OpCodes.Sub
                    | Mul -> OpCodes.Mul
                    | Div -> OpCodes.Div
                    | And -> OpCodes.And
                    | Or -> OpCodes.Or
                    | LShift -> OpCodes.Shl
                    | RShift -> OpCodes.Shr_Un
                    | ArShift -> OpCodes.Shr
                    | Xor -> OpCodes.Xor)
            | Negate(x) ->
                emitExp x
                il.Emit(OpCodes.Neg)
            | NewRecord(x) -> il.Emit(OpCodes.Newobj, (getOrCreateRecordType x).GetConstructor(Type.EmptyTypes))
            | NewArray(ty, size) ->
                emitExp size
                il.Emit(OpCodes.Newarr, reflectionType ty)
            | Var(x) -> failwith "Not implemented yet"
            | Field(x, record, field) ->
                emitExp x
                il.Emit(OpCodes.Ldfld, (getOrCreateRecordType record).GetField(field))
            | ArrayElem(arr, idx) ->
                emitExp arr
                emitExp idx
                il.Emit(
                    match Types.actualTy(getExpTy arr) with
                    | Types.Array(Types.Int) -> OpCodes.Ldelem_I4
                    | Types.Array(_) -> OpCodes.Ldelem_Ref
                    | _ -> failwith "ldelem: not an array")
            | CallExp(_, _) -> failwith "Not implemented yet"
            | CallStaticMethodExp(m, args, _) ->
                List.iter emitExp args
                il.EmitCall(OpCodes.Call, m, null)
            | ESeq(x, y) ->
                emitStm x
                emitExp y

        and emitStm =
            function
            | Store(left, right) -> failwith "Not implemented yet"
            | ExpStm(x) ->
                emitExp x
                il.Emit(OpCodes.Pop)
            | CallStm(_, _) -> failwith "Not implemented yet"
            | CallStaticMethodStm(m, args) ->
                List.iter emitExp args
                il.EmitCall(OpCodes.Call, m, null)
            | Jump(x) -> il.Emit(OpCodes.Br, getLabel x)
            | CJump(op, left, right, t, f) ->
                emitExp left
                emitExp right
                let t, f = getLabel t, getLabel f
                let j oc =
                    il.Emit(oc, t)
                    il.Emit(OpCodes.Br, f)
                match op with
                | Eq -> j OpCodes.Beq
                | Ne ->
                    il.Emit(OpCodes.Beq, f)
                    il.Emit(OpCodes.Br, t)
                | Lt -> j OpCodes.Blt
                | Gt -> j OpCodes.Bgt
                | Le -> j OpCodes.Ble
                | Ge -> j OpCodes.Bge
                | ULt -> j OpCodes.Blt_Un
                | ULe -> j OpCodes.Ble_Un
                | UGt -> j OpCodes.Bgt_Un
                | UGe -> j OpCodes.Bge_Un
            | Seq(x, y) ->
                emitStm x
                emitStm y
            | Label(x) ->
                il.MarkLabel(getLabel x)
            | Nop -> ()
            | Ret(x) ->
                match x with | Some(y) -> emitExp y | None -> ()
                il.Emit(OpCodes.Ret)
                
        List.iter emitStm l.Body
