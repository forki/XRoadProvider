﻿namespace XRoad

open FSharp.Core
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open XRoad.Attributes

[<RequireQualifiedAccessAttribute>]
module XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xrd = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"
    let [<Literal>] Xtee = "http://x-tee.riik.ee/xsd/xtee.xsd"

type DeserializerDelegate = delegate of XmlReader -> obj
type SerializerDelegate = delegate of XmlWriter * obj -> unit

type TypeMap =
    { Type: Type
      Deserializer: DeserializerDelegate
      Serializer: SerializerDelegate
      DeserializerMethod: MethodInfo option
      SerializerMethod: MethodInfo option }
    static member ForSystemType<'T>(deserializer: XmlReader -> obj, serializer: XmlWriter -> obj -> unit) =
        { Type = typeof<'T>
          Deserializer = DeserializerDelegate(deserializer)
          Serializer = SerializerDelegate(serializer)
          DeserializerMethod = None
          SerializerMethod = None }

type Serializer() as this =
    static let typeMaps = ConcurrentDictionary<Type, TypeMap>()

    static do
        typeMaps.TryAdd(typeof<string>, TypeMap.ForSystemType<string>((fun r -> r.ReadElementString() |> box), (fun wr v -> wr.WriteValue(v)))) |> ignore
        typeMaps.TryAdd(typeof<int32>, TypeMap.ForSystemType<int32>((fun r -> XmlConvert.ToInt32(r.ReadElementString()) |> box), (fun wr v -> wr.WriteValue(v)))) |> ignore

    let getTypeMap (typ: Type) =
        match typ.GetCustomAttribute<XRoadTypeAttribute>() with
        | null -> None
        | _ -> Some(this.GetTypeMap(typ))

    let getSubTypes (typ: Type) =
        let subTypes =
            typ.Assembly.GetTypes()
            |> Array.filter (fun x -> x.IsSubclassOf(typ))
            |> List.ofArray
        let rec orderTypes (ordered: Type list) (unordered: Type list) =
            let next, rem = unordered |> List.partition (fun x -> ordered |> List.exists ((=) x.BaseType))
            let newOrdered = next @ ordered
            match rem with
            | [] -> newOrdered
            | _ -> orderTypes newOrdered rem
        match subTypes with
        | [] -> []
        | xs -> orderTypes [typ] xs |> List.choose (getTypeMap) |> List.filter (fun x -> not x.Type.IsAbstract)

    static let objGetType = typeof<obj>.GetMethod("GetType", [| |])
    static let typeGetFullName = typeof<Type>.GetProperty("FullName").GetGetMethod()
    static let stringEquals = typeof<String>.GetMethod("Equals", [| typeof<string> |])

    let generate (il: ILGenerator) (property: PropertyInfo) (f: Expr<XmlWriter> -> Expr<obj> -> Expr<XmlWriter * obj -> unit> -> Expr<XmlWriter * obj -> unit> -> Expr<unit>) =
        let typeMap = getTypeMap property.PropertyType
        let subTypes = getSubTypes property.PropertyType
        let writer = Expr.GlobalVar("w")
        let value = Expr.GlobalVar("o")
        let serializer = Expr.GlobalVar("s")
        let attributes = Expr.GlobalVar("a")
        let expr = f writer value serializer attributes
        let rec genIL expr =
            let genArg (argsExpr: Expr list) =
                let rec genSingleArg (argExpr: Expr) =
                    match argExpr with
                    | e when e = (upcast value) ->
                        il.Emit(OpCodes.Ldarg_1)
                        il.Emit(OpCodes.Castclass, property.DeclaringType)
                        il.Emit(OpCodes.Callvirt, property.GetGetMethod())
                        if property.PropertyType.IsValueType then
                            il.Emit(OpCodes.Box, property.PropertyType)
                    | e when e = (upcast writer) -> il.Emit(OpCodes.Ldarg_0)
                    | Value(null, _) -> il.Emit(OpCodes.Ldnull)
                    | Value(value, typ) when typ = typeof<string> -> il.Emit(OpCodes.Ldstr, unbox<string> value)
                    | Coerce(e, _) -> genSingleArg e
                    | Call(_) -> genIL argExpr
                    | PropertyGet(_) -> genIL argExpr
                    | _ -> failwithf "Unimplemented expression: %A (%A)" argExpr expr
                argsExpr |> List.iter (genSingleArg)
            match expr with
            | Application(targetExpr, NewTuple(_)) when targetExpr = (attributes :> Expr) ->
                match subTypes with
                | [] -> ()
                | _ ->
                    let conditionEnd = il.DefineLabel()
                    let rec genSubType (lbl: Label option) subTypes =
                        match subTypes with
                        | [] -> ()
                        | x::xs ->
                            match lbl with Some(lbl) -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop) | None -> ()
                            let lbl = match xs with [] -> conditionEnd | _ -> il.DefineLabel()
                            genArg [value]
                            il.Emit(OpCodes.Call, objGetType)
                            il.Emit(OpCodes.Callvirt, typeGetFullName)
                            il.Emit(OpCodes.Ldstr, x.Type.FullName)
                            il.Emit(OpCodes.Callvirt, stringEquals)
                            il.Emit(OpCodes.Ldc_I4_0)
                            il.Emit(OpCodes.Ceq)
                            il.Emit(OpCodes.Brtrue_S, lbl)
                            il.Emit(OpCodes.Nop)
                            genIL <@ (%writer).WriteStartAttribute("type", XmlNamespace.Xsi) @>
                            let attr = x.Type.GetCustomAttribute<XRoadTypeAttribute>()
                            let typeName = match attr.Name with null | "" -> x.Type.Name | name -> name
                            match attr.Namespace with
                            | null | "" -> genIL <@ (%writer).WriteString(typeName) @>
                            | ns -> genIL <@ (%writer).WriteQualifiedName(typeName, ns) @>
                            genIL <@ (%writer).WriteEndAttribute() @>
                            il.Emit(OpCodes.Br, conditionEnd)
                            genSubType (Some lbl) xs
                    genArg [value]
                    il.Emit(OpCodes.Call, objGetType)
                    il.Emit(OpCodes.Callvirt, typeGetFullName)
                    il.Emit(OpCodes.Ldstr, property.PropertyType.FullName)
                    il.Emit(OpCodes.Callvirt, stringEquals)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                    il.Emit(OpCodes.Brfalse, conditionEnd)
                    il.Emit(OpCodes.Nop)
                    subTypes |> genSubType None
                    il.MarkLabel(conditionEnd)
            | Application(targetExpr, NewTuple(argsExpr)) when targetExpr = (serializer :> Expr) ->
                match subTypes with
                | [] ->
                    genArg argsExpr
                    il.Emit(OpCodes.Call, typeMap.Value.SerializerMethod.Value)
                    il.Emit(OpCodes.Nop)
                | _ ->
                    let conditionEnd = il.DefineLabel()
                    let rec genSubType (lbl: Label option) subTypes =
                        match subTypes with
                        | [] -> ()
                        | x::xs ->
                            match lbl with Some(lbl) -> il.MarkLabel(lbl); il.Emit(OpCodes.Nop) | None -> ()
                            let lbl = match xs with [] -> conditionEnd | _ -> il.DefineLabel()
                            genArg [value]
                            il.Emit(OpCodes.Call, objGetType)
                            il.Emit(OpCodes.Callvirt, typeGetFullName)
                            il.Emit(OpCodes.Ldstr, x.Type.FullName)
                            il.Emit(OpCodes.Callvirt, stringEquals)
                            il.Emit(OpCodes.Ldc_I4_0)
                            il.Emit(OpCodes.Ceq)
                            il.Emit(OpCodes.Brtrue_S, lbl)
                            il.Emit(OpCodes.Nop)
                            genArg argsExpr
                            il.Emit(OpCodes.Call, x.SerializerMethod.Value)
                            il.Emit(OpCodes.Nop)
                            il.Emit(OpCodes.Nop)
                            il.Emit(OpCodes.Br, conditionEnd)
                            genSubType (Some lbl) xs
                    subTypes |> genSubType None
                    il.MarkLabel(conditionEnd)
            | Call(Some(instExpr), mi, argsExpr) ->
                genArg (instExpr :: argsExpr)
                il.Emit(OpCodes.Callvirt, mi)
                il.Emit(OpCodes.Nop)
            | SpecificCall <@@ raise @@> (_, _, [argsExpr]) ->
                genIL argsExpr
                il.Emit(OpCodes.Throw)
            | SpecificCall <@@ (=) @@> (_, _, argsExpr) ->
                genArg argsExpr
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
            | Call(None, mi, argsExpr) ->
                genArg argsExpr
                il.Emit(OpCodes.Call, mi)
            | IfThenElse(condExpr, trueExpr, falseExpr) ->
                let lbl1 = il.DefineLabel()
                let lbl2 = il.DefineLabel()
                genIL condExpr
                il.Emit(OpCodes.Brtrue_S, lbl1)
                genIL trueExpr
                il.Emit(OpCodes.Br, lbl2)
                il.MarkLabel(lbl1)
                il.Emit(OpCodes.Nop)
                genIL falseExpr
                il.MarkLabel(lbl2)
                il.Emit(OpCodes.Nop)
            | NewObject(ci, [argsExpr]) ->
                genIL argsExpr
                il.Emit(OpCodes.Newobj, ci)
            | Sequential(expr1, expr2) ->
                genIL expr1
                genIL expr2
            | PropertyGet(Some(targetExpr), pi, argsExpr) ->
                genArg (targetExpr::argsExpr)
                il.Emit(OpCodes.Callvirt, pi.GetGetMethod())
                il.Emit(OpCodes.Nop)
            | _ -> failwithf "Unimplemented expression: %A" expr
        genIL expr

    member __.Deserialize<'T>(reader: XmlReader) : 'T =
        this.DeserializeObject<'T>(reader)

    member __.Serialize(writer: XmlWriter, value: obj, rootName: XmlQualifiedName) =
        match rootName.Namespace with
        | null | "" -> writer.WriteStartElement(rootName.Name)
        | _ -> writer.WriteStartElement(rootName.Name, rootName.Namespace)
        this.SerializeObject(writer, value)
        writer.WriteEndElement()

    member private __.DeserializeObject<'T>(reader: XmlReader) : 'T =
        match reader.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.map (fun x -> x.ToLower()) with
        | Some("true") | Some("1") -> Unchecked.defaultof<'T>
        | _ -> this.GetTypeMap(typeof<'T>).Deserializer.Invoke(reader) |> unbox<'T>

    member private __.SerializeObject(writer: XmlWriter, value: obj) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ -> this.GetTypeMap(value.GetType()).Serializer.Invoke(writer, value)

    member private __.GetTypeMap(typ) : TypeMap =
        match typeMaps.TryGetValue(typ) with
        | true, typeMap -> typeMap
        | false, _ -> this.BuildTypeMap(typ)

    member private __.BuildTypeMap(typ: Type) =
        let attr = typ.GetCustomAttributes(typeof<XRoadTypeAttribute>, false)
                    |> Array.map (fun a -> a :?> XRoadTypeAttribute)
                    |> Array.tryFind (fun _ -> true)
        match attr with
        | Some(_) ->
            let methodName = sprintf "%s_DynamicSerialize" typ.FullName
            let f = DynamicMethod(methodName, null, [| typeof<XmlWriter>; typeof<obj> |])
            let il = f.GetILGenerator()

            let rec callBase (typ: Type) =
                match getTypeMap typ.BaseType with
                | Some({ SerializerMethod = Some(mi) }) ->
                    callBase typ.BaseType
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Call, mi)
                    il.Emit(OpCodes.Nop)
                | _ -> ()
            callBase typ
            typ.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
            |> Array.choose (fun p ->
                if p.GetCustomAttribute<XRoadElementAttribute>() |> isNull && p.GetCustomAttribute<XRoadContentAttribute>() |> isNull
                then None
                else Some(p, attr))
            |> Array.iter (fun (p,_) -> this.BuildPropertySerialization(il, p))
            il.Emit(OpCodes.Ret)

            typeMaps.GetOrAdd(typ, { Type = typ
                                     Deserializer = DeserializerDelegate((fun _ -> failwith "not implemented"))
                                     Serializer = f.CreateDelegate(typeof<SerializerDelegate>) :?> SerializerDelegate
                                     DeserializerMethod = None
                                     SerializerMethod = Some(upcast f) })
        | None -> failwithf "Type `%s` is not serializable." typ.FullName

    member private __.BuildPropertySerialization(il: ILGenerator, property: PropertyInfo) =
        let contentAttribute = property.GetCustomAttribute<XRoadContentAttribute>() |> Option.ofObj
        let elementAttribute = property.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj
        generate il property
            (fun writer propValue serializer attributes ->
                let propertyName = property.Name
                let typeName = property.DeclaringType.FullName
                let nullExpr =
                    if elementAttribute |> Option.fold (fun _ x -> x.IsNullable) false
                    then <@ (%writer).WriteAttributeString("nil", XmlNamespace.Xsi, "true") @>
                    else <@ raise (Exception(String.Format("Not nullable property `{0}` of type `{1}` has null value.", propertyName, typeName))) @>
                let serializeExpr =
                    if property.PropertyType.GetCustomAttribute<XRoadTypeAttribute>() |> isNull then
                        if property.PropertyType = typeof<BigInteger> then
                            <@ (%writer).WriteValue((%propValue).ToString()) @>
                        else <@ (%writer).WriteValue((%propValue)) @>
                    else <@ (%serializer)((%writer), (%propValue)) @>
                let contentExpr =
                    if property.PropertyType.IsClass || Nullable.GetUnderlyingType(property.PropertyType) |> (isNull >> not) then
                        <@
                            if (%propValue) = null then (%nullExpr)
                            else
                                (%attributes)((%writer), (%propValue))
                                (%serializeExpr)
                        @>
                    else serializeExpr
                if contentAttribute |> Option.fold (fun _ _ -> true) false |> not then
                    <@
                        (%writer).WriteStartElement(propertyName)
                        (%contentExpr)
                        (%writer).WriteEndElement()
                     @>
                else contentExpr)
