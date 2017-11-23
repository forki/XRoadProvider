﻿module internal XRoad.Emitter

open Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open System.Xml.Linq
open XRoad
open XRoad.Serialization.Attributes

type Serialization =
    { Root: MethodInfo
      Content: MethodInfo }
    with
        static member Create (typ: Type): Serialization =
            { Root = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_SerializeContent" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true) }

type Deserialization =
    { Root: MethodInfo
      Content: MethodInfo
      MatchType: MethodInfo }
    with
        static member Create (typ: Type): Deserialization =
            { Root = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, typeof<bool>, [| typeof<XmlReader>; typeof<obj>; typeof<bool>; typeof<SerializerContext> |], true)
              MatchType = DynamicMethod(sprintf "%s_MatchType" typ.FullName, typeof<bool>, [| typeof<XmlReader> |], true) }

type TypeMap =
    { Type: Type
      Name: string
      Namespace: string option
      Layout: LayoutKind option
      IsAnonymous: bool
      DeserializeDelegate: Lazy<DeserializerDelegate>
      Deserialization: Deserialization
      SerializeDelegate: Lazy<SerializerDelegate>
      Serialization: Serialization
      CanHaveNullAsValue: bool
      BaseType: TypeMap option
      mutable IsComplete: bool }
    member this.Serialize(writer: XmlWriter, value: obj, context: SerializerContext) =
        this.SerializeDelegate.Value.Invoke(writer, value, context)
    member this.Deserialize(reader: XmlReader, context: SerializerContext) =
        this.DeserializeDelegate.Value.Invoke(reader, context)
    member this.FullName =
        match this.Namespace with
        | Some(ns) -> sprintf "%s:%s" ns this.Name
        | None -> this.Name
    static member Create(typ: Type, deserialization, serialization, baseType) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>() |> Option.ofObj
        let layout = attr |> Option.map (fun attr -> attr.Layout)
        { Type = typ
          Name = attr |> Option.fold (fun name attr -> match attr.Name with null | "" -> name | x -> x) typ.Name
          Namespace = attr |> Option.bind (fun attr -> match attr.Namespace with null | "" -> None | x -> Some(x))
          Layout = layout
          IsAnonymous = attr |> Option.map (fun attr -> attr.IsAnonymous) |> MyOption.defaultValue false
          Deserialization = deserialization
          DeserializeDelegate = lazy (deserialization.Root.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.Root.CreateDelegate(typeof<SerializerDelegate>) |> unbox)
          CanHaveNullAsValue = (not (Nullable.GetUnderlyingType(typ) |> isNull)) || (typ.IsClass && layout <> Some(LayoutKind.Choice))
          BaseType = baseType
          IsComplete = false }

type ContentWrapper =
    | Choice of TypeMap * FieldInfo * FieldInfo * int * MethodInfo
    | Method of MethodInfo * int32
    | Type of TypeMap
    member this.Name
        with get () =
            match this with Choice (tm,_,_,_,_) -> tm.Type.FullName | Method (mi, _) -> sprintf "%s.%s" mi.DeclaringType.FullName mi.Name | Type tm -> tm.FullName

type PropertyMap =
    { TypeMap: TypeMap
      SimpleTypeName: XmlQualifiedName option
      Element: (XName * bool * bool) option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }

type ArrayMap =
    { Type: Type
      Element: (XName * bool * bool) option
      ItemTypeMap: TypeMap
      ItemElement: (XName * bool * bool) option
      ItemSimpleTypeName: XmlQualifiedName option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }
    member this.GetItemPropertyMap() =
        { TypeMap = this.ItemTypeMap
          SimpleTypeName = this.ItemSimpleTypeName
          Element = this.ItemElement
          Wrapper = this.Wrapper
          GetMethod = None
          SetMethod = None
          HasValueMethod = None }

type Property =
    | Individual of PropertyMap
    | Array of ArrayMap
    member this.Element with get() = this |> function Individual x -> x.Element | Array x -> x.Element
    member this.Wrapper with get() = this |> function Individual x -> x.Wrapper | Array x -> x.Wrapper
    member this.Type with get() = this |> function Individual x -> x.TypeMap.Type | Array x -> x.Type
    member this.GetMethod with get() = this |> function Individual x -> x.GetMethod | Array x -> x.GetMethod
    member this.PropertyName
        with get() =
            match this with
            | Individual { Element = Some(name,_,_) }
            | Array { Element = Some(name,_,_) }
            | Array { Element = None; ItemElement = Some(name,_,_) } -> Some(name)
            | _ -> None
    member this.SimpleTypeName
        with get() =
            match this with
            | Individual(x) -> x.SimpleTypeName
            | Array(_) -> Some(XmlQualifiedName("Array", XmlNamespace.SoapEnc))
    member this.HasValueMethod with get() = match this with | Individual(x) -> x.HasValueMethod | Array(x) -> x.HasValueMethod

let firstRequired (properties: Property list) =
    properties
    |> List.tryPick (fun p -> match p.Element with Some(_,_,false) -> Some(p) | _ -> None)

let (!~>) (mi: MethodInfo) = match mi with :? DynamicMethod as dyn -> dyn | _ -> failwith "Cannot cast to dynamic method."

type private XopBinaryContent() =
    inherit BinaryContent("", Data [| |])

let (|Serializable|NotSerializable|) (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> NotSerializable
    | attr -> Serializable(attr)

let (!@) expr =
    match expr with
    | Call(_, mi, _) -> mi
    | PropertyGet(_, pi, _) -> pi.GetGetMethod()
    | _ -> failwithf "Must be method call expression, but was `%A`." expr

let (!!@) expr =
    match expr with
    | NewObject(ci, _) -> ci
    | _ -> failwith "Must be constructor expression"

let safe (name: XName) = if name.NamespaceName = "" then name.LocalName else sprintf "%s:%s" name.NamespaceName name.LocalName

type PropertyInput = ContentWrapper * string * Type * bool * MethodInfo option * MethodInfo option * MethodInfo option * XRoadElementAttribute * XRoadCollectionAttribute option

let getContentOfType (typeMap: TypeMap) : PropertyInput list =
    typeMap.Type.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    |> List.ofArray
    |> List.sortBy (fun p -> p.MetadataToken)
    |> List.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> List.map
        (fun (p, attr) ->
            let propertyType, isOptionalType, hasValueMethod =
                if p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    p.PropertyType.GenericTypeArguments.[0], true, Some(p.PropertyType.GetProperty("HasValue").GetGetMethod())
                else p.PropertyType, false, None
            Type typeMap, p.Name, propertyType, isOptionalType, hasValueMethod, Some(p.GetGetMethod()), Some(p.GetSetMethod(true)), attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)

let getContentOfMethod (mi: MethodInfo) : PropertyInput list =
    mi.GetParameters()
    |> Seq.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> Seq.mapi (fun i (p, attr) ->
        let parameterType, isOptionalType, hasValueMethod =
            if p.ParameterType.IsGenericType && p.ParameterType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                p.ParameterType.GenericTypeArguments.[0], true, Some(p.ParameterType.GetProperty("HasValue").GetGetMethod())
            else p.ParameterType, false, None
        Method(mi, i), p.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)
    |> Seq.toList

let getContentOfChoice (choiceMap: TypeMap) : PropertyInput list =
    let choiceType = choiceMap.Type
    let idField =
        match choiceType.GetField("__id", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__id@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    let valueField =
        match choiceType.GetField("__value", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__value@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    choiceType.GetCustomAttributes<XRoadElementAttribute>()
    |> Seq.map
        (fun attr ->
            let (typ, mi) =
                let methodName = sprintf "New%s%s" (if Char.IsLower(attr.Name.[0]) then "_" else "") attr.Name
                match choiceType.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static) with
                | null -> failwithf "Type `%s` should define public static method `%s`." choiceType.FullName methodName
                | mi -> match mi.GetParameters() with
                        | [| pi |] -> (pi.ParameterType, mi)
                        | _ -> failwithf "Type `%s` method `New%s` should have exactly one argument." choiceType.FullName attr.Name
            let parameterType, isOptionalType, hasValueMethod =
                if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    typ.GenericTypeArguments.[0], true, Some(typ.GetProperty("HasValue").GetGetMethod())
                else typ, false, None
            let collectionAttr = choiceType.GetCustomAttributes<XRoadCollectionAttribute>() |> Seq.tryFind (fun a -> a.Id = attr.Id)
            Choice (choiceMap, idField, valueField, attr.Id, mi), attr.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, collectionAttr)
    |> Seq.toList

module EmitSerialization =
    /// Check if values type matches expected type.
    let private emitValueTypeTest (il: ILGenerator) (expectedType: Type) =
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, !@ <@ (null: obj).GetType() @>)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: Type).FullName @>)
        il.Emit(OpCodes.Ldstr, expectedType.FullName)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)

    /// Write type attribute according to TypeMap.
    let emitTypeAttribute (il: ILGenerator) (typeName: string) (typeNamespace: string option) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "type")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartAttribute("", "") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, typeName)
        match typeNamespace with
        | Some(ns) -> il.Emit(OpCodes.Ldstr, ns)
                      il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteQualifiedName("", "") @>)
        | None ->     il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteString("") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndAttribute() @>)
        il.Emit(OpCodes.Nop)

    /// Emit type (and its base types) content serialization.
    let rec private emitContentSerialization (il: ILGenerator) (typeMap: TypeMap) =
        typeMap.BaseType |> Option.iter (emitContentSerialization il)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Ldarg_2)
        il.Emit(OpCodes.Call, typeMap.Serialization.Content)
        il.Emit(OpCodes.Nop)

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (il: ILGenerator) (typeMap: TypeMap) =
        il.Emit(OpCodes.Ldstr, sprintf "Cannot serialize abstract type `%s`." typeMap.FullName)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

    /// Emit whole contents of TypeMap serialization.
    let private emitBodySerialization (il: ILGenerator) addType (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then
            typeMap |> emitAbstractTypeException il
        else
            if addType then emitTypeAttribute il typeMap.Name typeMap.Namespace
            // TODO : Attributes
            typeMap |> emitContentSerialization il

    /// Emit serialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchySerialization il (markReturn: Label) isEncoded (subTypes: TypeMap list) typeMap =
        match subTypes with
        | [] ->
            typeMap |> emitBodySerialization il isEncoded
        | subType::other ->
            let markNext = il.DefineLabel()

            // Check if type matches current TypeMap.
            subType.Type |> emitValueTypeTest il
            il.Emit(OpCodes.Brfalse, markNext)

            subType |> emitBodySerialization il true

            il.Emit(OpCodes.Br, markReturn)
            il.MarkLabel(markNext)
            il.Emit(OpCodes.Nop)
            typeMap |> emitTypeHierarchySerialization il markReturn isEncoded other

    let emitNilAttribute (markReturn: Label) (il: ILGenerator) =
        let markNotNull = il.DefineLabel()
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse, markNotNull)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "nil")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Ldstr, "true")
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteAttributeString("", "", "") @>)
        il.Emit(OpCodes.Nop)
        il.Emit(OpCodes.Br, markReturn)
        il.MarkLabel(markNotNull)
        il.Emit(OpCodes.Nop)

    /// Emit root type serialization logic for given TypeMap.
    let emitRootSerializerMethod (il: ILGenerator) isEncoded (subTypes: TypeMap list) (typeMap: TypeMap) =
        let markReturn = il.DefineLabel()

        // When value is `null`, write `xsi:nil` attribute and return.
        il.Emit(OpCodes.Ldarg_1)
        il |> emitNilAttribute markReturn

        // Serialize value according to its type.
        typeMap |> emitTypeHierarchySerialization il markReturn isEncoded subTypes

        // Return
        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

    /// Provides value for array item at current index.
    let private emitArrayItemValue (il: ILGenerator) (array: LocalBuilder) (index: LocalBuilder) (typ: Type) =
        il.Emit(OpCodes.Ldloc, array)
        il.Emit(OpCodes.Ldloc, index)
        il.Emit(OpCodes.Ldelem, typ)
        if (typ.IsValueType) then
            il.Emit(OpCodes.Box, typ)

    /// Emit validation for not nullable types to have value specified.
    let private emitNotNullableCheck (il: ILGenerator) (name: string) emitValue property =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            let markSuccess = il.DefineLabel()

            // Check if value is null.
            emitValue property.Type
            il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Ceq)
            il.Emit(OpCodes.Brfalse, markSuccess)

            // Not nullable shouldn't have null as value, so throw exception.
            il.Emit(OpCodes.Ldstr, sprintf "Not nullable property `%s` of type `%s` has null value." name property.Wrapper.Name)
            il.Emit(OpCodes.Newobj, !!@ <@ Exception("") @>)
            il.Emit(OpCodes.Throw)

            il.MarkLabel(markSuccess)
            il.Emit(OpCodes.Nop)
        | _ -> ()

    let emitPropertyWrapperSerialization (property: Property) (il: ILGenerator) =
        match property.Wrapper with
        | Choice (_,_,fld,_,_) ->
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Castclass, fld.DeclaringType)
            il.Emit(OpCodes.Ldfld, fld)
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            match property.Type.IsValueType with
            | true -> il.Emit(OpCodes.Unbox_Any, ty)
            | _ -> il.Emit(OpCodes.Castclass, ty)
        | Method (mi, i) ->
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldc_I4, i)
            il.Emit(OpCodes.Ldelem_Ref)
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> MyOption.defaultWith (fun _ -> property.Type)
            match ty.IsValueType with
            | true -> il.Emit(OpCodes.Unbox_Any, ty)
            | _ -> il.Emit(OpCodes.Castclass, ty)
        | Type tm ->
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Castclass, tm.Type)
            il.Emit(OpCodes.Callvirt, property.GetMethod.Value)

    let emitOptionalFieldSerialization (property: Property) (emitContent: unit -> unit) (il: ILGenerator) =
        match property.Element with
        | Some(_,_,true) ->
            let endContentLabel = il.DefineLabel()
            let optionalType = il.DeclareLocal(property.HasValueMethod.Value.DeclaringType)

            il |> emitPropertyWrapperSerialization property
            il.Emit(OpCodes.Stloc, optionalType)
            il.Emit(OpCodes.Ldloca, optionalType)
            il.Emit(OpCodes.Call, property.HasValueMethod.Value)
            il.Emit(OpCodes.Brfalse, endContentLabel)
            il.Emit(OpCodes.Nop)

            emitContent()

            il.MarkLabel(endContentLabel)
            il.Emit(OpCodes.Nop)
        | _ ->
            emitContent()

    /// Emit single property content serialization.
    let rec emitPropertyContentSerialization (il: ILGenerator) (emitValue: Type -> unit) isEncoded (property: Property) =
        let markReturn = il.DefineLabel()

        // Write start element of the propery if its not merged with content.
        match property.Element with
        | Some(name, isNullable, _) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldstr, name.LocalName)
            il.Emit(OpCodes.Ldstr, name.NamespaceName)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("", "") @>)
            il.Emit(OpCodes.Nop)
            if not isNullable then property |> emitNotNullableCheck il name.LocalName emitValue
            if isEncoded then
                property.SimpleTypeName
                |> Option.iter (fun typeName -> emitTypeAttribute il typeName.Name (Some(typeName.Namespace)))
        | None -> ()

        // Serialize property content value according to its TypeMap.
        match property with
        | Individual propertyMap ->
            il.Emit(OpCodes.Ldarg_0)
            emitValue propertyMap.TypeMap.Type
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, propertyMap.TypeMap.Serialization.Root)
            il.Emit(OpCodes.Nop)
        | Array arrayMap ->
            emitValue arrayMap.Type
            il |> emitNilAttribute markReturn
            let arr = il.DeclareLocal(arrayMap.Type)
            emitValue arrayMap.Type
            il.Emit(OpCodes.Stloc, arr)
            let i = il.DeclareLocal(typeof<int>)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Stloc, i)
            let markLoopCondition = il.DefineLabel()
            il.Emit(OpCodes.Br, markLoopCondition)
            let markLoopStart = il.DefineLabel()
            il.MarkLabel(markLoopStart)
            il.Emit(OpCodes.Nop)
            let itemPropertyMap = Individual (arrayMap.GetItemPropertyMap())
            let itemEmitValue = emitArrayItemValue il arr i
            emitPropertyContentSerialization il itemEmitValue isEncoded itemPropertyMap
            il.Emit(OpCodes.Ldloc, i)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Add)
            il.Emit(OpCodes.Stloc, i)
            il.MarkLabel(markLoopCondition)
            il.Emit(OpCodes.Ldloc, i)
            il.Emit(OpCodes.Ldloc, arr)
            il.Emit(OpCodes.Ldlen)
            il.Emit(OpCodes.Conv_I4)
            il.Emit(OpCodes.Clt)
            il.Emit(OpCodes.Brtrue, markLoopStart)

        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Nop)

        // Write end element if required.
        match property.Element with
        | Some(_) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndElement() @>)
            il.Emit(OpCodes.Nop)
        | None -> ()

    /// Unbox property value into correct type.
    let emitPropertyValue (il: ILGenerator) (property: Property) (typ: Type) =
        il |> emitPropertyWrapperSerialization property
        match property.Element with
        | Some(_,_,true) ->
            let opt = il.DeclareLocal(property.HasValueMethod.Value.DeclaringType)
            il.Emit(OpCodes.Stloc, opt)
            il.Emit(OpCodes.Ldloca, opt)
            if typ.IsValueType then
                let temp = il.DeclareLocal(typ)
                il.Emit(OpCodes.Ldloca, temp)
                il.Emit(OpCodes.Initobj, typ)
                il.Emit(OpCodes.Ldloc, temp)
            else il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Call, property.HasValueMethod.Value.DeclaringType.GetMethod("ValueOr", [| typ |]))
        | _ -> ()
        if typ.IsValueType then
            il.Emit(OpCodes.Box, typ)

    /// Emit IL which serializes each property value into corresponding xml fragment.
    let emitContentSerializerMethod (il: ILGenerator) isEncoded (properties: Property list) =
        properties
        |> List.iter (fun property -> 
            il |> emitOptionalFieldSerialization property (fun () -> property |> emitPropertyContentSerialization il (emitPropertyValue il property) isEncoded))

module EmitDeserialization =
    /// Check if current element has `xsi:nil` attribute present.
    let emitNullCheck (markReturn: Label) (il: ILGenerator) =
        let nilValue = il.DeclareLocal(typeof<string>)

        // Get attribute value into local variable, in case of null empty string is used.
        let markSkipNull = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "nil")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).GetAttribute("", "") @>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Brtrue, markSkipNull)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ldstr, "")
        il.MarkLabel(markSkipNull)
        il.Emit(OpCodes.Callvirt, !@ <@ "".ToLower() @>)
        il.Emit(OpCodes.Stloc, nilValue)

        // When attribute value is "true" or "1" return null.
        let markNull = il.DefineLabel()
        let markNotNull = il.DefineLabel()
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "1")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brtrue, markNull)
        il.Emit(OpCodes.Ldloc, nilValue)
        il.Emit(OpCodes.Ldstr, "true")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brtrue, markNull)
        il.Emit(OpCodes.Br, markNotNull)

        // return null;
        il.MarkLabel(markNull)
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Br, markReturn)
        il.MarkLabel(markNotNull)
        il.Emit(OpCodes.Nop)

    /// Emit type (and its base types) content deserialization.
    let rec private emitContentDeserialization (instance: LocalBuilder) (typeMap: TypeMap) (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, instance)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Content)

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) (il: ILGenerator) =
        il.Emit(OpCodes.Ldstr, sprintf "Cannot deserialize abstract type `%s`." typeMap.FullName)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

    let private emitXmlReaderRead (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Read() @>)
        il.Emit(OpCodes.Nop)

    let private emitXmlReaderReadOrExcept (propertyName: XName option) (il: ILGenerator) =
        let markSuccess = il.DefineLabel()
        il |> emitXmlReaderRead
        il.Emit(OpCodes.Brtrue, markSuccess)
        let errorMessage =
            match propertyName with
            | Some(name) -> sprintf "Invalid message: expected `%s`, but was end of file." (safe name)
            | None -> "Invalid message: unexpected end of file."
        il.Emit(OpCodes.Ldstr, errorMessage)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)
        il.MarkLabel(markSuccess)
        il.Emit(OpCodes.Nop)

    let private emitMoveToEndOrNextElement (skipVar: LocalBuilder, depthVar: LocalBuilder) name (il: ILGenerator) =
        let doneLabel = il.DefineLabel()
        let skipReadLabel = il.DefineLabel()
        il.Emit(OpCodes.Ldloc, skipVar)
        il.Emit(OpCodes.Brtrue, skipReadLabel)
        il |> emitXmlReaderReadOrExcept name
        il.MarkLabel(skipReadLabel)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Stloc, skipVar)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
        il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.EndElement)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse, doneLabel)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldloc, depthVar)
        il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Brfalse, doneLabel)
        doneLabel

    let emitWrongElementException expectedValue wrapper (il: ILGenerator) =
        let wrapperName =
            match wrapper with
            | Choice _ -> sprintf "choice `%s`" wrapper.Name
            | Method _ -> sprintf "operation `%s` wrapper element" wrapper.Name
            | Type _ -> sprintf "type `%s`" wrapper.Name 
        il.Emit(OpCodes.Ldstr, sprintf "Element `%s` was expected in subsequence of %s, but element `{0}` was found instead." expectedValue wrapperName)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
        il.Emit(OpCodes.Call, !@ <@ String.Format("", "") @>)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

    /// Emit whole contents of TypeMap deserialization.
    let private emitBodyDeserialization (returnLabel: Label) (skipVar: LocalBuilder) (typeMap: TypeMap) (il: ILGenerator) =
        if typeMap.Type.IsAbstract then
            il |> emitAbstractTypeException typeMap
        else
            // Declare local variable to hold result.
            let instance = il.DeclareLocal(typeMap.Type)
            let ctor = typeMap.Type.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [| |], [| |])
            il.Emit(OpCodes.Newobj, ctor)
            il.Emit(OpCodes.Stloc, instance)
            // TODO : Attributes
            il |> emitContentDeserialization instance typeMap
            il.Emit(OpCodes.Stloc, skipVar)
            il.Emit(OpCodes.Ldloc, instance)

    /// Check if value type matches expected type.
    let private emitValueTypeTest isDefault (typeName: LocalBuilder, typeNamespace: LocalBuilder) (markNext: Label) (typeMap: TypeMap) (il: ILGenerator) =
        let secondChanceLabel = if isDefault && not typeMap.IsAnonymous then Some(il.DefineLabel()) else None
        il.Emit(OpCodes.Ldloc, typeName)
        il.Emit(OpCodes.Ldstr, if typeMap.IsAnonymous then "" else typeMap.Name)
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brfalse, secondChanceLabel |> MyOption.defaultValue markNext)
        il.Emit(OpCodes.Ldloc, typeNamespace)
        il.Emit(OpCodes.Ldstr, if typeMap.IsAnonymous then "" else typeMap.Namespace |> Option.fold (fun _ x -> x) "")
        il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
        il.Emit(OpCodes.Brfalse, secondChanceLabel |> MyOption.defaultValue markNext)
        secondChanceLabel |> Option.iter
            (fun label ->
                let skip = il.DefineLabel()
                il.Emit(OpCodes.Br, skip)
                il.MarkLabel(label)
                il.Emit(OpCodes.Ldloc, typeName)
                il.Emit(OpCodes.Ldstr, "")
                il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                il.Emit(OpCodes.Brfalse, markNext)
                il.Emit(OpCodes.Ldloc, typeNamespace)
                il.Emit(OpCodes.Ldstr, "")
                il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                il.Emit(OpCodes.Brfalse, markNext)
                il.MarkLabel(skip)
                il.Emit(OpCodes.Nop))

    /// Emit deserialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchyDeserialization (markReturn: Label) (skipVar: LocalBuilder) (subTypes: TypeMap list) typeName typeMap (il: ILGenerator) =
        match subTypes with
        | [] ->
            let errorLabel = il.DefineLabel()
            il |> emitValueTypeTest true typeName errorLabel typeMap
            il |> emitBodyDeserialization markReturn skipVar typeMap
            il.Emit(OpCodes.Br, markReturn)
            il.MarkLabel(errorLabel)
            let context = if typeMap.IsAnonymous then "anonymous type" else sprintf "type `%s`" (XName.Get(typeMap.Name, typeMap.Namespace |> MyOption.defaultValue "") |> safe)
            il.Emit(OpCodes.Ldstr, sprintf "Unexpected type value: using type `{0}:{1}` is not allowed in the context of %s." context)
            il.Emit(OpCodes.Ldloc, snd typeName)
            il.Emit(OpCodes.Ldloc, fst typeName)
            il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
            il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
            il.Emit(OpCodes.Throw)
        | subType::other ->
            let markNext = il.DefineLabel()

            // Check if type matches current TypeMap.
            il |> emitValueTypeTest false typeName markNext subType

            // Deserialize content
            il |> emitBodyDeserialization markReturn skipVar subType

            il.Emit(OpCodes.Br, markReturn)
            il.MarkLabel(markNext)
            il.Emit(OpCodes.Nop)
            il |> emitTypeHierarchyDeserialization markReturn skipVar other typeName typeMap

    /// Reads type attribute value and stores name and namespace in variables.
    let private emitTypeAttributeRead (typeMap: TypeMap) (il: ILGenerator) =
        let typeName = il.DeclareLocal(typeof<string>)
        let typeNamespace = il.DeclareLocal(typeof<string>)

        let markParse = il.DefineLabel()
        let markDone = il.DefineLabel()
        let markDefaultNamespace = il.DefineLabel()
        let markWithPrefix = il.DefineLabel()

        // Load empty string as default values.
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Stloc, typeNamespace)

        // When `xsi:type` is not present use default values.
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "type")
        il.Emit(OpCodes.Ldstr, XmlNamespace.Xsi)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).GetAttribute("", "") @>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Brtrue, markParse)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Br, markDone)

        // Parse `xsi:type` value into type name and namespace.
        il.MarkLabel(markParse)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Newarr, typeof<char>)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldc_I4, int32 ':')
        il.Emit(OpCodes.Stelem_I2)
        il.Emit(OpCodes.Ldc_I4_2)
        il.Emit(OpCodes.Callvirt, !@ <@ "".Split([| ':' |], 2) @>)
        il.Emit(OpCodes.Dup)

        // When default namespace is used (no prefix).
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Conv_I4)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse, markWithPrefix)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Br, markDefaultNamespace)

        // When prefix is present.
        il.MarkLabel(markWithPrefix)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeName)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldelem_Ref)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldloc, typeNamespace)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)
        il.Emit(OpCodes.Br, markDone)

        // Use default namespace when no prefix was found.
        il.MarkLabel(markDefaultNamespace)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, "")
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LookupNamespace("") @>)
        il.Emit(OpCodes.Stloc, typeNamespace)

        il.MarkLabel(markDone)
        il.Emit(OpCodes.Nop)

        (typeName,typeNamespace)

    let emitRootDeserializerMethod hasInlineContent (subTypes: TypeMap list) (typeMap: TypeMap) (il: ILGenerator) =
        let markReturn = il.DefineLabel()
        
        let depthVar = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stloc, depthVar)

        // When value nil attribute is present returns null.
        il |> emitNullCheck markReturn

        // Read type attribute value of current element.
        let typeName = il |> emitTypeAttributeRead typeMap

        // Serialize value according to its type.
        let skipVar = il.DeclareLocal(typeof<bool>)
        il |> emitTypeHierarchyDeserialization markReturn skipVar subTypes typeName typeMap
        il.MarkLabel(markReturn)
        
        if not hasInlineContent then
            let startLabel = il.DefineLabel()
    
            il.MarkLabel(startLabel)
            let doneLabel = il |> emitMoveToEndOrNextElement (skipVar, depthVar) None
    
            let successLabel = il.DefineLabel()
            il.Emit(OpCodes.Br, successLabel)
            il.MarkLabel(doneLabel)
    
            // reader.Depth != depth
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
            il.Emit(OpCodes.Ldloc, depthVar)
            il.Emit(OpCodes.Ceq)
            il.Emit(OpCodes.Brfalse, startLabel)
    
            // reader.NodeType != XmlNodeType.Element
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
            il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.Element)
            il.Emit(OpCodes.Ceq)
            il.Emit(OpCodes.Brfalse, startLabel)
            
            il |> emitWrongElementException "<end of element>" (Type typeMap)
    
            il.MarkLabel(successLabel)
        il.Emit(OpCodes.Ret)

    let emitPropertyValueDeserialization (isContent: bool) (typeMap: TypeMap) (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(if isContent then OpCodes.Ldarg_3 else OpCodes.Ldarg_1)
        il.Emit(OpCodes.Call, typeMap.Deserialization.Root)
        match typeMap.Type.IsValueType with
        | true -> il.Emit(OpCodes.Unbox_Any, typeMap.Type)
        | _ -> il.Emit(OpCodes.Castclass, typeMap.Type)

    let emitPropertyWrapperDeserialization (wrapper: ContentWrapper) (il: ILGenerator) =  
        match wrapper with
        | Choice _ -> ()
        | Method _ -> failwith "Method signature is not deserialzable"
        | Type tm ->
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Castclass, tm.Type)

    let emitIndividualPropertyDeserialization isContent (propertyMap: PropertyMap) (il: ILGenerator) =
        let x = il.DeclareLocal(propertyMap.TypeMap.Type)
        il |> emitPropertyValueDeserialization isContent propertyMap.TypeMap
        il.Emit(OpCodes.Stloc, x)
        il |> emitPropertyWrapperDeserialization propertyMap.Wrapper
        il.Emit(OpCodes.Ldloc, x)
        match propertyMap.Element with
        | Some(_,_,true) ->
            let m =
                typeof<Optional.Option>.GetMethods()
                |> Array.filter (fun m -> m.Name = "Some" && m.GetGenericArguments().Length = 1)
                |> Array.exactlyOne
            let m = m.MakeGenericMethod([| propertyMap.TypeMap.Type |])
            il.Emit(OpCodes.Call, m)
        | _ -> ()

    let emitArrayContentEndCheck (markArrayEnd: Label) (skipVar: LocalBuilder, depthVar: LocalBuilder) (il: ILGenerator) =
        let markSuccess = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
        il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.EndElement)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse, markSuccess)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldloc, depthVar)
        il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Brfalse, markSuccess)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Stloc, skipVar)
        il.Emit(OpCodes.Br, markArrayEnd)
        il.MarkLabel(markSuccess)
        il.Emit(OpCodes.Nop)

    let emitXmlReaderDepthCheck (varDepth: LocalBuilder) (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Ldloc, varDepth)
        il.Emit(OpCodes.Ceq)

    let emitXmlReaderNodeTypeCheck (il: ILGenerator) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ceq)

    let emitArrayItemDeserialization isContent (arrayMap: ArrayMap, listInstance: LocalBuilder, markEnd: Label, stopIfWrongElement) (il: ILGenerator) =
        match arrayMap.ItemElement with
        | Some(name,_,_) ->
            let markDeserialize = il.DefineLabel()
            let markError = il.DefineLabel()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
            il.Emit(OpCodes.Ldstr, name.LocalName)
            il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
            il.Emit(OpCodes.Brfalse, markError)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NamespaceURI @>)
            il.Emit(OpCodes.Ldstr, name.NamespaceName)
            il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
            il.Emit(OpCodes.Brtrue, markDeserialize)
            il.MarkLabel(markError)
            if stopIfWrongElement then
                il.Emit(OpCodes.Br, markEnd)
            else
                il.Emit(OpCodes.Ldstr, "Unexpected element: found `{0}`, but was expecting to find `{1}`.")
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                il.Emit(OpCodes.Ldstr, safe name)
                il.Emit(OpCodes.Call, !@ <@ String.Format("", "", "") @>)
                il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
                il.Emit(OpCodes.Throw)
            il.MarkLabel(markDeserialize)
        | None -> ()
        il.Emit(OpCodes.Ldloc, listInstance)
        il |> emitPropertyValueDeserialization isContent arrayMap.ItemTypeMap
        il.Emit(OpCodes.Callvirt, listInstance.LocalType.GetMethod("Add", [| arrayMap.ItemTypeMap.Type |]))

    /// Emits array type deserialization logic.
    let emitArrayPropertyDeserialization isContent skipVar (arrayMap: ArrayMap) (il: ILGenerator) =
        let depthVar = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Stloc, depthVar)

        let markArrayEnd = il.DefineLabel()
        let markArrayNull = il.DefineLabel()

        if arrayMap.Element.IsSome then
            il |> emitNullCheck markArrayNull
            il.Emit(OpCodes.Ldloc, depthVar)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Add)
            il.Emit(OpCodes.Stloc, depthVar)

        let listType = typedefof<System.Collections.Generic.List<_>>.MakeGenericType(arrayMap.ItemTypeMap.Type)
        let listInstance = il.DeclareLocal(listType)

        il.Emit(OpCodes.Newobj, listType.GetConstructor([| |]))
        il.Emit(OpCodes.Stloc, listInstance)

        let markSkipRead = il.DefineLabel()

        // Empty element has nothing to parse.
        if arrayMap.Element.IsSome then
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).IsEmptyElement @>)
            il.Emit(OpCodes.Brtrue, markArrayEnd)
        else il.Emit(OpCodes.Br, markSkipRead)

        let markLoopStart = il.DefineLabel()

        il.MarkLabel(markLoopStart)
        il |> emitXmlReaderReadOrExcept (arrayMap.ItemElement |> Option.map (fun (x,_,_) -> x))
        il.MarkLabel(markSkipRead)

        il |> emitArrayContentEndCheck markArrayEnd (skipVar, depthVar)

        il |> emitXmlReaderDepthCheck depthVar
        il.Emit(OpCodes.Brfalse, markLoopStart)

        il |> emitXmlReaderNodeTypeCheck
        il.Emit(OpCodes.Brfalse, markLoopStart)

        il |> emitArrayItemDeserialization isContent (arrayMap, listInstance, markArrayEnd, arrayMap.Element.IsNone)
        il.Emit(OpCodes.Br, markLoopStart)

        il.MarkLabel(markArrayEnd)

        let instance = il.DeclareLocal(arrayMap.Type)

        il.Emit(OpCodes.Ldloc,listInstance)
        il.Emit(OpCodes.Callvirt, listType.GetMethod("ToArray", [| |]))

        il.MarkLabel(markArrayNull)
        il.Emit(OpCodes.Stloc, instance)

        il |> emitPropertyWrapperDeserialization arrayMap.Wrapper
        il.Emit(OpCodes.Ldloc, instance)

        match arrayMap.Element with
        | Some(_,_,true) ->
            let m =
                typeof<Optional.Option>.GetMethods()
                |> Array.filter (fun m -> m.Name = "Some" && m.GetGenericArguments().Length = 1)
                |> Array.exactlyOne
            il.Emit(OpCodes.Call, m.MakeGenericMethod([| arrayMap.Type |]))
        | _ -> ()

    let emitMatchType property (il: ILGenerator) =
        match property with
        | Some(Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap })
        | Some(Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeMap.Deserialization.MatchType)
        | None
        | Some(Individual { Element = None })
        | Some(Array { Element = None; ItemElement = None }) ->
            il.Emit(OpCodes.Ldc_I4_0)
        | Some(Individual { Element = Some(name,_,_) })
        | Some(Array { Element = Some(name,_,_) })
        | Some(Array { Element = None; ItemElement = Some(name,_,_) }) ->
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
            il.Emit(OpCodes.Ldstr, name.LocalName)
            il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NamespaceURI @>)
            il.Emit(OpCodes.Ldstr, name.NamespaceName)
            il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
            il.Emit(OpCodes.Add)
            il.Emit(OpCodes.Ldc_I4_2)
            il.Emit(OpCodes.Div)

    let emitPropertyDeserialization skipVar (prop: Property) (il: ILGenerator) =
        let setMethod =
            match prop with
            | Individual propertyMap ->
                il |> emitIndividualPropertyDeserialization true propertyMap
                propertyMap.SetMethod
            | Array arrayMap ->
                il |> emitArrayPropertyDeserialization true skipVar arrayMap
                arrayMap.SetMethod
        il.Emit(OpCodes.Callvirt, setMethod.Value)

    let emitSequenceDeserialization (startLabel: Label, returnLabel: Label) (skipRead: LocalBuilder, depthVar: LocalBuilder) (properties: Property list) (il: ILGenerator) =
        let rec emitPropertyDeser startLabel (propList: Property list) =
            match propList with
            | [] -> ()
            | prop::xs ->
                il.MarkLabel(startLabel)

                let markSuccess2 = il |> emitMoveToEndOrNextElement (skipRead, depthVar) prop.PropertyName

                match propList |> firstRequired with
                | Some(p) ->
                    let expectedName =
                        match p with
                        | Individual { Element = Some(name,_,_) } | Array { Element = Some(name,_,_) } | Array { ItemElement = Some(name,_,_) } -> safe name
                        | _ -> "<end of sequence>"
                    il |> emitWrongElementException expectedName prop.Wrapper
                | None -> il.Emit(OpCodes.Br, returnLabel)

                il.MarkLabel(markSuccess2)
                il.Emit(OpCodes.Nop)

                // reader.Depth != depth
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
                il.Emit(OpCodes.Ldloc, depthVar)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, startLabel)

                // reader.NodeType != XmlNodeType.Element
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NodeType @>)
                il.Emit(OpCodes.Ldc_I4, int32 XmlNodeType.Element)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, startLabel)

                let nextLabel = match xs with [] -> returnLabel | _ -> il.DefineLabel()

                match prop with
                | Individual { Element = Some(name,_,isOptional) }
                | Array { Element = Some(name,_,isOptional) } ->
                    // reader.LocalName != property.Name
                    let markDeserialize = il.DefineLabel()
                    let markError = il.DefineLabel()
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, name.LocalName)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Brfalse, markError)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NamespaceURI @>)
                    il.Emit(OpCodes.Ldstr, name.NamespaceName)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Brtrue, markDeserialize)
                    il.MarkLabel(markError)
                    if isOptional then
                        il.Emit(OpCodes.Ldc_I4_1)
                        il.Emit(OpCodes.Stloc, skipRead)
                        il.Emit(OpCodes.Br, nextLabel)
                    else il |> emitWrongElementException (safe name) prop.Wrapper
                    il.MarkLabel(markDeserialize)
                | _ -> ()

                // Deserialize property
                il |> emitPropertyDeserialization skipRead prop

                emitPropertyDeser nextLabel xs
        match properties with
        | [] ->
            il.MarkLabel(startLabel)
            il.Emit(OpCodes.Nop)
        | _ ->
            properties |> emitPropertyDeser startLabel

let (|InlineContent|_|) (properties: Property list) =
    match properties with
    | [Individual({ Element = None; TypeMap = typeMap }) as prop]
    | [Array({ Element = None; ItemElement = None; ItemTypeMap = typeMap }) as prop] ->
        match typeMap.Layout with
        | Some(LayoutKind.Choice) -> None
        | _ -> Some(prop)
    | _ -> None

let rec private createDeserializeContentMethodBody (il: ILGenerator) (typeMap: TypeMap) (properties: Property list) =
    let returnLabel = il.DefineLabel()

    let skipRead = il.DeclareLocal(typeof<bool>)
    il.Emit(OpCodes.Ldarg_2)
    il.Emit(OpCodes.Stloc, skipRead)

    match properties with
    | InlineContent(prop) ->
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Stloc, skipRead)
        il |> EmitDeserialization.emitPropertyDeserialization skipRead prop
    | _ ->
        let varDepth = il.DeclareLocal(typeof<int>)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).Depth @>)
        il.Emit(OpCodes.Stloc, varDepth)

        let startLabel = il.DefineLabel()

        il.Emit(OpCodes.Ldloc, skipRead)
        il.Emit(OpCodes.Brtrue, startLabel)
        il.Emit(OpCodes.Ldloc, varDepth)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stloc, varDepth)

        typeMap.BaseType
        |> Option.iter
            (fun typeMap ->
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Ldloc, skipRead)
                il.Emit(OpCodes.Ldarg_3)
                il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
                il.Emit(OpCodes.Stloc, skipRead))

        match typeMap.Layout.Value with
        | LayoutKind.Choice ->
            ()
        | LayoutKind.Sequence ->
            let requiredProperty = properties |> firstRequired
            let label = il.DefineLabel()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).IsEmptyElement @>)
            match requiredProperty with
            | Some(p) ->
                let (name,_,_) = p.Element |> Option.get
                il.Emit(OpCodes.Brfalse, label)
                il |> EmitDeserialization.emitWrongElementException (safe name) (Type typeMap)
            | None ->
                il.Emit(OpCodes.Brtrue, returnLabel)
            il.MarkLabel(label)
            il.Emit(OpCodes.Nop)
            EmitDeserialization.emitSequenceDeserialization (startLabel, returnLabel) (skipRead, varDepth) properties il
        | _ -> failwith "Not implemented"

    il.MarkLabel(returnLabel)
    il.Emit(OpCodes.Ldloc, skipRead)
    il.Emit(OpCodes.Ret)

and createTypeSerializers isEncoded (typeMap: TypeMap) =
    let properties = getContentOfType typeMap |> getProperties (getTypeMap isEncoded)
    let directSubTypes = typeMap.Type |> findDirectSubTypes isEncoded

    // Emit serializers
    let ilSer = (!~> typeMap.Serialization.Root).GetILGenerator()
    EmitSerialization.emitRootSerializerMethod ilSer isEncoded directSubTypes typeMap

    let ilSerContent = (!~> typeMap.Serialization.Content).GetILGenerator()
    EmitSerialization.emitContentSerializerMethod ilSerContent isEncoded properties
    ilSerContent.Emit(OpCodes.Ret)

    let hasInlineContent = match properties with InlineContent _ -> true | _ -> false

    // Emit deserializers
    let ilDeser = (!~> typeMap.Deserialization.Root).GetILGenerator()
    ilDeser |> EmitDeserialization.emitRootDeserializerMethod hasInlineContent directSubTypes typeMap

    let ilDeserContent = (!~> typeMap.Deserialization.Content).GetILGenerator()
    createDeserializeContentMethodBody ilDeserContent typeMap properties

    match properties with
    | [Individual { Element = None }] | [Array { Element = None; ItemElement = None }] ->
        ()
    | _ ->
        let ilMatch = (!~> typeMap.Deserialization.MatchType).GetILGenerator()
        ilMatch |> EmitDeserialization.emitMatchType (properties |> List.tryHead)
        ilMatch.Emit(OpCodes.Ret)

and createChoiceTypeSerializers isEncoded (properties: Property list) (choiceMap: TypeMap) =
    let genSerialization () =
        let il = (!~> choiceMap.Serialization.Root).GetILGenerator()
        let conditionEnd = il.DefineLabel()
        let rec generate (label: Label option) (properties: Property list) =
            match properties with
            | [] -> ()
            | property::other ->
                let idField, id = match property.Wrapper with Choice(_, idField, _, id, _) -> idField, id | _ -> failwith "never"

                label |> Option.iter (fun label -> il.MarkLabel(label); il.Emit(OpCodes.Nop))
                let label = match other with [] -> conditionEnd | _ -> il.DefineLabel()

                il.Emit(OpCodes.Ldarg_1)
                il.Emit(OpCodes.Castclass, choiceMap.Type)
                il.Emit(OpCodes.Ldfld, idField)
                il.Emit(OpCodes.Ldc_I4_S, id)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, label)
                il.Emit(OpCodes.Nop)

                EmitSerialization.emitOptionalFieldSerialization
                    property
                    (fun () ->
                        EmitSerialization.emitPropertyContentSerialization
                            il
                            (EmitSerialization.emitPropertyValue il property)
                            isEncoded
                            property)
                    il

                il.Emit(OpCodes.Nop)
                il.Emit(OpCodes.Br, conditionEnd)

                generate (Some label) other

        generate None properties

        il.MarkLabel(conditionEnd)
        il.Emit(OpCodes.Ret)

    let genContentDeserialization () =
        let il = (!~> choiceMap.Deserialization.Content).GetILGenerator()
        il.Emit(OpCodes.Ldarg_2)
        il.Emit(OpCodes.Ret)

    let genDeserialization () =
        let il = (!~> choiceMap.Deserialization.Root).GetILGenerator()

        let markReturn = il.DefineLabel()
        
        let skipRead = il.DeclareLocal(typeof<bool>)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Stloc, skipRead)

        let rec generate (properties: Property list) =
            match properties with
            | [] -> ()
            | property::other ->
                let mi = match property.Wrapper with Choice(_, _, _, _, mi) -> mi | _ -> failwith "never"

                let label = il.DefineLabel()

                match property with
                | Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                | Individual { Element = None; TypeMap = typeMap }
                | Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                | Array { Element = None; ItemElement = None; ItemTypeMap = typeMap } ->
                    let instance = il.DeclareLocal(property.Type)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.MatchType)
                    il.Emit(OpCodes.Brfalse, label)
                    il.Emit(OpCodes.Newobj, typeMap.Type.GetConstructor([| |]))
                    il.Emit(OpCodes.Stloc, instance)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldloc, instance)
                    il.Emit(OpCodes.Ldloc, skipRead)
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Call, typeMap.Deserialization.Content)
                    il.Emit(OpCodes.Stloc, skipRead)
                    il.Emit(OpCodes.Ldloc, instance)
                | Individual { Element = Some(name,_,_) }
                | Array { Element = Some(name,_,_) }
                | Array { Element = None; ItemElement = Some(name,_,_) } ->
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
                    il.Emit(OpCodes.Ldstr, name.LocalName)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).NamespaceURI @>)
                    il.Emit(OpCodes.Ldstr, name.NamespaceName)
                    il.Emit(OpCodes.Call, !@ <@ "" = "" @>)
                    il.Emit(OpCodes.Add)
                    il.Emit(OpCodes.Ldc_I4_2)
                    il.Emit(OpCodes.Div)
                    il.Emit(OpCodes.Brfalse, label)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Stloc, skipRead)
                    match property with
                    | Individual propertyMap -> il |> EmitDeserialization.emitIndividualPropertyDeserialization false propertyMap
                    | Array arrayMap -> il |> EmitDeserialization.emitArrayPropertyDeserialization false skipRead arrayMap

                il.Emit(OpCodes.Call, mi)
                il.Emit(OpCodes.Br, markReturn)
                il.MarkLabel(label)
                il.Emit(OpCodes.Nop)
                generate other
        generate properties

        let names = properties |> List.map (fun p -> p.PropertyName)
        let errorMessage = sprintf "Invalid message: expected one of %A, but `{0}` was found instead." names
        il.Emit(OpCodes.Ldstr, errorMessage)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlReader).LocalName @>)
        il.Emit(OpCodes.Call, !@ <@ String.Format("", "") @>)
        il.Emit(OpCodes.Newobj, typeof<Exception>.GetConstructor([| typeof<string> |]))
        il.Emit(OpCodes.Throw)

        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

    let genMatch () =
        let il = (!~> choiceMap.Deserialization.MatchType).GetILGenerator()
        let markReturn = il.DefineLabel()

        let rec generate (properties: Property list) =
            match properties with
            | [] ->
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Br, markReturn)
            | property::other ->
                il |> EmitDeserialization.emitMatchType (Some property)
                let label = il.DefineLabel()
                il.Emit(OpCodes.Brfalse, label)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Br, markReturn)
                il.MarkLabel(label)
                il.Emit(OpCodes.Nop)
                generate other
        generate properties

        il.MarkLabel(markReturn)
        il.Emit(OpCodes.Ret)

    genSerialization ()
    genContentDeserialization ()
    genDeserialization ()
    genMatch ()

and private getProperties (tmf: Type -> TypeMap) (input: PropertyInput list) : Property list =
    input
    |> List.map
        (fun (wrapper, propName, propertyType, isOptionalType, hasValueMethod, getMethod, setMethod, attr, cattr) ->
            let name = match attr.Name with null | "" -> propName | name -> name
            let xname = match attr.Namespace with "" -> XName.Get(name) | ns -> XName.Get(name, ns)
            let element = if attr.MergeContent then None else Some(xname, attr.IsNullable, isOptionalType)
            if propertyType.IsArray then
                let elementType = propertyType.GetElementType()
                let itemTypeMap = (if attr.UseXop then typeof<XopBinaryContent> else elementType) |> tmf
                let itemName = cattr |> Option.bind (fun a -> match a.ItemName with null | "" -> None | name -> Some(name)) |> MyOption.defaultWith (fun _ -> "item")
                let itemXName = cattr |> Option.bind (fun a -> match a.ItemNamespace with "" -> None | ns -> Some(XName.Get(itemName, ns))) |> MyOption.defaultWith (fun _ -> XName.Get(itemName))
                let itemElement = if itemTypeMap.Layout <> Some(LayoutKind.Choice)
                                  then if cattr.IsSome && cattr.Value.MergeContent then None else Some(itemXName, cattr.IsSome && cattr.Value.ItemIsNullable, true)
                                  else None
                Array { Type = propertyType
                        Element = element
                        ItemTypeMap = itemTypeMap
                        ItemElement = itemElement
                        ItemSimpleTypeName = XRoadHelper.getSystemTypeName elementType.FullName
                        Wrapper = wrapper
                        GetMethod = getMethod
                        SetMethod = setMethod
                        HasValueMethod = hasValueMethod }
            else
                let propertyTypeMap = (if attr.UseXop then typeof<XopBinaryContent> else propertyType) |> tmf
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Individual { TypeMap = propertyTypeMap
                             SimpleTypeName = XRoadHelper.getSystemTypeName (propertyType.FullName)
                             Element = element
                             Wrapper = wrapper
                             GetMethod = getMethod
                             SetMethod = setMethod
                             HasValueMethod = hasValueMethod })

and private typeMaps = ConcurrentDictionary<Type, TypeMap>()
and private uncompleteTypeMaps = ConcurrentDictionary<Type, TypeMap>()

and private createTypeMap (isEncoded: bool) (typ: Type) =
    let addTypeMap (init: TypeMap -> unit) (typ: Type) =
        let serialization, deserialization = typ |> Serialization.Create, typ |> Deserialization.Create
        let typeMap = TypeMap.Create(typ, deserialization, serialization, typ |> findBaseType isEncoded)
        if typeMaps.TryAdd(typ, typeMap) then
            uncompleteTypeMaps.TryAdd(typ, typeMap) |> ignore
            try
                typeMap |> init
            // with
            //     TODO: generate exceptions for invalid typemap methods.
            finally
                uncompleteTypeMaps.TryRemove(typ) |> ignore
                typeMap.IsComplete <- true
            typeMap
        else typeMaps.[typ]
    match typ with
    | NotSerializable ->
        failwithf "Type `%s` is not serializable." typ.FullName
    | Serializable(typeAttribute) ->
        typ |> addTypeMap (fun typeMap ->
            match typeAttribute.Layout with
            | LayoutKind.Choice -> typeMap |> createChoiceTypeSerializers isEncoded (getContentOfChoice typeMap |> getProperties (getTypeMap isEncoded))
            | _ -> typeMap |> createTypeSerializers isEncoded)

and internal getTypeMap (isEncoded: bool) (typ: Type) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> typ |> createTypeMap isEncoded

and findTypeMap isEncoded (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(typ |> getTypeMap isEncoded)

and findBaseType isEncoded (typ: Type) =
    if typ.BaseType |> isNull || typ.BaseType = typeof<obj> then None
    else match typ.BaseType |> findTypeMap isEncoded with
         | None -> typ.BaseType |> findBaseType isEncoded
         | typeMap -> typeMap

and findDirectSubTypes (isEncoded: bool) (typ: Type) : TypeMap list =
    typ.Assembly.GetTypes()
    |> List.ofArray
    |> List.filter (fun x -> x.BaseType = typ)
    |> List.choose (findTypeMap isEncoded)

let getCompleteTypeMap isEncoded typ =
    let typeMap = getTypeMap isEncoded typ
    while uncompleteTypeMaps.Count > 0 do
        System.Threading.Thread.Sleep(100)
    typeMap

module internal XsdTypes =
    open NodaTime
    open NodaTime.Text
    open System.IO

    let serializeDefault (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value)

    let serializeBigInteger (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value.ToString())

    let serializeString (writer: XmlWriter, value: obj, _: SerializerContext) =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        elif unbox value = "" then ()
        else writer.WriteValue(value)

    let serializeNullable (writer: XmlWriter) (value: obj) (context: SerializerContext) fser =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        else fser(writer, value, context)

    let serializeLocalDate (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(LocalDatePattern.Iso.Format(unbox value))

    let serializeLocalDateTime (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(LocalDateTimePattern.GeneralIso.Format(unbox value))

    let deserializeNullable (reader: XmlReader) (context: SerializerContext) fdeser =
        let nilValue = reader.GetAttribute("nil", XmlNamespace.Xsi)
        let nilValue = if nilValue |> isNull then "" else nilValue.ToLower()
        if nilValue = "1" || nilValue = "true" then null else fdeser(reader, context)

    let deserializeValue<'T> (reader: XmlReader) (_: SerializerContext) (fread: unit -> 'T) : obj =
        if reader.IsEmptyElement then box Unchecked.defaultof<'T>
        elif reader.Read() then fread() |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeStringValue (reader: XmlReader, _: SerializerContext) : obj =
        if reader.IsEmptyElement then box ""
        elif reader.Read() then reader.ReadContentAsString() |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeDateTimeValue value =
        match LocalDateTimePattern.ExtendedIso.Parse(value) with
        | result when result.Success -> result.Value
        | _ -> OffsetDateTimePattern.ExtendedIso.Parse(value).GetValueOrThrow().LocalDateTime

    let serializeNullableDefault (writer, value, context) = serializeNullable writer value context serializeDefault
    let serializeNullableBigInteger (writer, value, context) = serializeNullable writer value context serializeBigInteger
    let serializeNullableLocalDate (writer, value, context) = serializeNullable writer value context serializeLocalDate
    let serializeNullableLocalDateTime (writer, value, context) = serializeNullable writer value context serializeLocalDateTime

    let deserializeBoolean (reader, context) = deserializeValue reader context reader.ReadContentAsBoolean
    let deserializeDecimal (reader, context) = deserializeValue reader context reader.ReadContentAsDecimal
    let deserializeDouble (reader, context) = deserializeValue reader context reader.ReadContentAsDouble
    let deserializeInt32 (reader, context) = deserializeValue reader context reader.ReadContentAsInt
    let deserializeInt64 (reader, context) = deserializeValue reader context reader.ReadContentAsLong
    let deserializeBigInteger (reader, context) = deserializeValue reader context (reader.ReadContentAsDecimal >> BigInteger)
    let deserializeLocalDate (reader, context) = deserializeValue reader context (fun () -> LocalDatePattern.Iso.Parse(reader.ReadContentAsString()).GetValueOrThrow())
    let deserializeLocalDateTime (reader, context) = deserializeValue reader context (fun () -> reader.ReadContentAsString() |> deserializeDateTimeValue)

    let deserializeNullableBoolean (reader, context) = deserializeNullable reader context deserializeBoolean
    let deserializeNullableDecimal (reader, context) = deserializeNullable reader context deserializeDecimal
    let deserializeNullableDouble (reader, context) = deserializeNullable reader context deserializeDouble
    let deserializeNullableInt32 (reader, context) = deserializeNullable reader context deserializeInt32
    let deserializeNullableInt64 (reader, context) = deserializeNullable reader context deserializeInt64
    let deserializeNullableBigInteger (reader, context) = deserializeNullable reader context deserializeBigInteger
    let deserializeNullableLocalDate (reader, context) = deserializeNullable reader context deserializeLocalDate
    let deserializeNullableLocalDateTime (reader, context) = deserializeNullable reader context deserializeLocalDateTime
    let deserializeString (reader, context) = deserializeNullable reader context deserializeStringValue

    let serializeBinaryContent (writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            if context.IsMultipart then
                context.Attachments.Add(content.ContentID, content)
                writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            else
                let bytes = (unbox<BinaryContent> value).GetBytes()
                writer.WriteBase64(bytes, 0, bytes.Length)

    let serializeXopBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            writer.WriteStartElement("xop", "Include", XmlNamespace.Xop)
            let content = unbox<BinaryContent> value
            context.Attachments.Add(content.ContentID, content)
            writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            writer.WriteEndElement()

    let deserializeBinaryContent (reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            match reader.GetAttribute("href") with
            | null ->
                if reader.IsEmptyElement then BinaryContent.Create([| |])
                else
                    reader.Read() |> ignore
                    let bufferSize = 4096
                    let buffer = Array.zeroCreate<byte>(bufferSize)
                    use stream = new MemoryStream()
                    let rec readContents() =
                        let readCount = reader.ReadContentAsBase64(buffer, 0, bufferSize)
                        if readCount > 0 then stream.Write(buffer, 0, readCount)
                        if readCount = bufferSize then readContents()
                    readContents()
                    stream.Flush()
                    stream.Position <- 0L
                    BinaryContent.Create(stream.ToArray())
            | contentID -> context.GetAttachment(contentID)

    let deserializeXopBinaryContent (reader: XmlReader, context: SerializerContext) =
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |])
            else
                let depth = reader.Depth + 1
                let rec moveToXopInclude () =
                    if reader.Read() then
                        if reader.NodeType = XmlNodeType.EndElement && reader.Depth < depth then false
                        elif reader.NodeType <> XmlNodeType.Element || reader.Depth <> depth || reader.LocalName <> "Include" || reader.NamespaceURI <> XmlNamespace.Xop then moveToXopInclude()
                        else true
                    else false
                if moveToXopInclude () then
                    match reader.GetAttribute("href") with
                    | null -> failwithf "Missing reference to multipart content in xop:Include element."
                    | contentID -> context.GetAttachment(contentID)
                else BinaryContent.Create([| |])

    let private addTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typ, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let private addBinaryTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typeof<BinaryContent>, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let mi e = match e with Call(_,mi,_) -> mi | _ -> failwith "do not use for that"

    let init () =
        addTypeMap typeof<bool> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeBoolean(null, null) @>)
        addTypeMap typeof<Nullable<bool>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableBoolean(null, null) @>)
        addTypeMap typeof<decimal> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDecimal(null, null) @>)
        addTypeMap typeof<Nullable<decimal>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDecimal(null, null) @>)
        addTypeMap typeof<double> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDouble(null, null) @>)
        addTypeMap typeof<Nullable<double>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDouble(null, null) @>)
        addTypeMap typeof<int32> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt32(null, null) @>)
        addTypeMap typeof<Nullable<int32>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt32(null, null) @>)
        addTypeMap typeof<int64> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt64(null, null) @>)
        addTypeMap typeof<Nullable<int64>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt64(null, null) @>)
        addTypeMap typeof<BigInteger> (mi <@ serializeBigInteger(null, null, null) @>) (mi <@ deserializeBigInteger(null, null) @>)
        addTypeMap typeof<Nullable<BigInteger>> (mi <@ serializeNullableBigInteger(null, null, null) @>) (mi <@ deserializeNullableBigInteger(null, null) @>)
        addTypeMap typeof<LocalDate> (mi <@ serializeLocalDate(null, null, null) @>) (mi <@ deserializeLocalDate(null, null) @>)
        addTypeMap typeof<Nullable<LocalDate>> (mi <@ serializeNullableLocalDate(null, null, null) @>) (mi <@ deserializeNullableLocalDate(null, null) @>)
        addTypeMap typeof<LocalDateTime> (mi <@ serializeLocalDateTime(null, null, null) @>) (mi <@ deserializeLocalDateTime(null, null) @>)
        addTypeMap typeof<Nullable<LocalDateTime>> (mi <@ serializeNullableLocalDateTime(null, null, null) @>) (mi <@ deserializeNullableLocalDateTime(null, null) @>)
        addTypeMap typeof<string> (mi <@ serializeString(null, null, null) @>) (mi <@ deserializeString(null, null) @>)
        addBinaryTypeMap typeof<BinaryContent> (mi <@ serializeBinaryContent(null, null, null) @>) (mi <@ deserializeBinaryContent(null, null) @>)
        addBinaryTypeMap typeof<XopBinaryContent> (mi <@ serializeXopBinaryContent(null, null, null) @>) (mi <@ deserializeXopBinaryContent(null, null) @>)

module internal DynamicMethods =
    let requiredOpAttr<'T when 'T :> Attribute and 'T : null and 'T : equality> (mi: MethodInfo) : 'T =
        mi.GetCustomAttribute<'T>()
        |> Option.ofObj
        |> MyOption.defaultWith (fun _ -> failwithf "Operation should define `%s`." typeof<'T>.Name)
        
    let emitDeserializer (mi: MethodInfo) (responseAttr: XRoadResponseAttribute) : DeserializerDelegate =
        let returnType = responseAttr.ReturnType |> Option.ofObj |> MyOption.defaultValue mi.ReturnType
        let typeMap = getCompleteTypeMap responseAttr.Encoded returnType
        typeMap.DeserializeDelegate.Value
        
    let emitSerializer (mi: MethodInfo) (requestAttr: XRoadRequestAttribute) : OperationSerializerDelegate =
        let method = 
            DynamicMethod
                ( sprintf "serialize_%s" mi.Name,
                  null,
                  [| typeof<XmlWriter>; typeof<obj[]>; typeof<SerializerContext> |],
                  true )

        let il = method.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldstr, requestAttr.Name)
        il.Emit(OpCodes.Ldstr, requestAttr.Namespace)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteStartElement("", "") @>)
    
        let parameters = getContentOfMethod mi |> getProperties (getCompleteTypeMap requestAttr.Encoded)
        EmitSerialization.emitContentSerializerMethod il requestAttr.Encoded parameters
    
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Callvirt, !@ <@ (null: XmlWriter).WriteEndElement() @>)
        il.Emit(OpCodes.Ret)
        
        method.CreateDelegate(typeof<OperationSerializerDelegate>) |> unbox
        
    let createMethodMap (mi: MethodInfo) : MethodMap =
        let operationAttr = mi |> requiredOpAttr<XRoadOperationAttribute>
        let requestAttr = mi |> requiredOpAttr<XRoadRequestAttribute>
        let responseAttr = mi |> requiredOpAttr<XRoadResponseAttribute>
        let requiredHeadersAttr = mi.GetCustomAttribute<XRoadRequiredHeadersAttribute>() |> Option.ofObj

        { Deserializer = emitDeserializer mi responseAttr
          Serializer = emitSerializer mi requestAttr
          Protocol = operationAttr.Protocol
          Request =
            { IsEncoded = requestAttr.Encoded
              IsMultipart = requestAttr.Multipart
              Accessor = Some(XmlQualifiedName(requestAttr.Name, requestAttr.Namespace)) }
          Response =
            { IsEncoded = responseAttr.Encoded
              IsMultipart = responseAttr.Multipart
              Accessor = Some(XmlQualifiedName(responseAttr.Name, responseAttr.Namespace)) }
          ServiceCode = operationAttr.ServiceCode
          ServiceVersion = operationAttr.ServiceVersion |> Option.ofObj
          Namespaces = []
          RequiredHeaders = dict [ match requiredHeadersAttr with
                                   | Some(attr) -> yield (attr.Namespace, attr.Names)
                                   | None -> () ] }

    let private methodMaps = ConcurrentDictionary<MethodInfo, MethodMap>()

    let internal getMethodMap mi =
        match methodMaps.TryGetValue(mi) with
        | true, mm -> mm
        | _ -> methodMaps.GetOrAdd(mi, (createMethodMap mi))

let getMethodMap = DynamicMethods.getMethodMap

do XsdTypes.init()
