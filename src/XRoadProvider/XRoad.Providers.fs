﻿module XRoad.Providers.Impl

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Reflection

/// Generated type providers for X-Road infrastructure.
/// Currently only one type provider is available, which builds service interface for certain producer.
[<TypeProvider>]
type XRoadProducerProvider() as this =
    let invalidation = Event<_,_>()

    let namespaceName = "XRoad.Providers"
    let theAssembly = typeof<XRoadProducerProvider>.Assembly

    // Already generated assemblies
    let typeCache = Dictionary<_,_>()

    // Available parameters to use for configuring type provider instance
    let staticParameters: ParameterInfo [] =
        let producerUriParam = ProvidedStaticParameter("ProducerUri", typeof<string>)
        producerUriParam.AddXmlDoc("WSDL document location (either local file or network resource).")
        let undescribedFaultsParam = ProvidedStaticParameter("UndescribedFaults", typeof<bool>, false)
        undescribedFaultsParam.AddXmlDoc("Generate code to handle service errors even if WSDL document doesn't explicitly define error responses.")
        let languageCodeParam = ProvidedStaticParameter("LanguageCode", typeof<string>, "et")
        languageCodeParam.AddXmlDoc("Specify language code that is extracted as documentation tooltips. Default value is estonian (et).")
        [| producerUriParam; undescribedFaultsParam; languageCodeParam |]

    interface ITypeProvider with
        /// Called when type alias is created, generates assembly for given arguments.
        override __.ApplyStaticArguments(typeWithoutArguments, typeNameWithArguments, staticArguments) =
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition ->
                match staticArguments with
                | [| :? string as producerUri; :? bool as undescribedFaults; :? string as languageCode |] ->
                    // Same parameter set should have same output, so caching is reasonable.
                    let key = (String.Join(".", typeNameWithArguments), producerUri, undescribedFaults, languageCode)
                    match typeCache.TryGetValue(key) with
                    | false, _ ->
                        let typ = XRoad.ProducerDefinition.makeProducerType(typeNameWithArguments, producerUri, undescribedFaults, languageCode)
                        typeCache.Add(key, typ)
                        typ
                    | true, typ -> typ
                | _ -> failwith "invalid type provider arguments"
            | _ -> failwith "not implemented"

        /// Returns contents of assembly generated by type provider instance.
        override __.GetGeneratedAssemblyContents(assembly) =
            File.ReadAllBytes(assembly.ManifestModule.FullyQualifiedName)

        /// Generated types need to handle only instantiation and method call expressions, others are not used.
        override __.GetInvokerExpression(syntheticMethodBase, parameters) =
            let parameters = parameters |> List.ofArray
            match syntheticMethodBase with
            | :? ConstructorInfo as ctor -> Expr.NewObject(ctor, parameters)
            | :? MethodInfo as mi -> Expr.Call(parameters.Head, mi, parameters.Tail)
            | _ -> failwith "not implemented"

        /// Namespaces provided by this type provider instance.
        override __.GetNamespaces() = [| this |]

        /// Exactly one type can have static parameters with current implementation.
        override __.GetStaticParameters(typeWithoutArguments) =
            match typeWithoutArguments with
            | :? ProvidedTypeDefinition as ty when ty.Name = typeWithoutArguments.Name -> staticParameters
            | _ -> [| |]

        /// Default implementation for invalidation event.
        [<CLIEvent>]
        override __.Invalidate = invalidation.Publish

        /// No unmanaged resources to deallocate.
        override __.Dispose() = ()

    interface IProvidedNamespace with
        /// No nested namespaces defined.
        override __.GetNestedNamespaces() = [| |]

        /// Type provider contains exactly one abstract type which allows access to type provider functionality.
        override __.GetTypes() =
            let producerType = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadProducer", Some(typeof<obj>), IsErased=false)
            producerType.AddXmlDoc("Type provider for generating service interfaces and data types for specific X-Road producer.")
            [| producerType |]

        /// Use default namespace for type provider namespace.
        override __.NamespaceName with get() = namespaceName

        /// No types have to be resolved.
        override __.ResolveTypeName(_) = null

/// Erased type providers for X-Road infrastructure.
/// Currently only one type provider is available, which acquires list of all producers from
/// security server.
[<TypeProvider>]
type XRoadProviders() as this =
    inherit TypeProviderForNamespaces()

    let theAssembly = typeof<XRoadProviders>.Assembly
    let namespaceName = "XRoad.Providers"
    let baseTy = typeof<obj>

    // Main type which provides access to producer list.
    let serverTy =
        let typ = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadServer", Some baseTy)
        typ.AddXmlDoc("Type provider which discovers available producers from specified X-Road security server.")
        typ

    do
        let serverIPParam = ProvidedStaticParameter("ServerIP", typeof<string>)
        serverIPParam.AddXmlDoc("IP address of X-Road security server which is used for producer discovery task.")

        serverTy.DefineStaticParameters(
            [ serverIPParam ],
            fun typeName parameterValues ->
                let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy)
                match parameterValues with
                | [| :? string as serverIP |] ->
                    // Create field which holds default service endpoint for the security server.
                    let requestUri = ProvidedLiteralField("RequestUri", typeof<string>, sprintf "http://%s/cgi-bin/consumer_proxy" serverIP)
                    thisTy.AddMember(requestUri)
                    // Create type which holds producer list.
                    let producersTy = ProvidedTypeDefinition("Producers", Some baseTy, HideObjectMethods=true)
                    producersTy.AddXmlDoc("List of available database names registered at the security server.")
                    thisTy.AddMember(producersTy)
                    // Add list of members which each corresponds to certain producer.
                    XRoad.SecurityServer.discoverProducers(serverIP)
                    |> List.map (fun producer ->
                        let producerTy = ProvidedTypeDefinition(producer.Name, Some baseTy, HideObjectMethods=true)
                        producerTy.AddMember(ProvidedLiteralField("ProducerName", typeof<string>, producer.Name))
                        producerTy.AddMember(ProvidedLiteralField("WsdlUri", typeof<string>, producer.WsdlUri))
                        producerTy.AddXmlDoc(producer.Description)
                        producerTy)
                    |> producersTy.AddMembers
                | _ -> failwith "Unexpected parameter values!"
                thisTy)

    // Main type for erased infrastructure access.
    let producerTy =
        let typ = ProvidedTypeDefinition(theAssembly, namespaceName, "XRoadProducerE", Some baseTy)
        typ.AddXmlDoc("Erased type provider for accessing individual methods of certain producer.")
        typ

    let cache = ConcurrentDictionary<_,_>()

    do
        let producerUriParam = ProvidedStaticParameter("ProducerUri", typeof<string>)
        producerUriParam.AddXmlDoc("WSDL document location (either local file or network resource).")

        let languageCodeParam = ProvidedStaticParameter("LanguageCode", typeof<string>, "et")
        languageCodeParam.AddXmlDoc("Specify language code that is extracted as documentation tooltips. Default value is estonian (et).")

        producerTy.DefineStaticParameters(
            [producerUriParam; languageCodeParam],
            fun typeName parameterValues ->
                match parameterValues with
                | [| :? string as uri; :? string as lang |] ->
                    let key = (uri, lang)
                    match cache.TryGetValue(key) with
                    | true, v -> v
                    | _ ->
                        let thisTy = ProvidedTypeDefinition(theAssembly, namespaceName, typeName, Some baseTy)
                        XRoad.ErasedProducerDefinition.initProducerMembers thisTy uri lang
                        cache.GetOrAdd(key, thisTy)
                | _ -> failwith "Unexpected parameter values."
            )

    do this.AddNamespace(namespaceName, [serverTy; producerTy])
