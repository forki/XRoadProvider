module private XRoad.ErasedProducerDefinition

open ProviderImplementation.ProvidedTypes
open System.Reflection
open XRoad.Common

let baseTy = typeof<obj>

// TODO: Erased type provider would benefit if schema was loaded and parsed lazily.
//       Current implementation loads and parses entire schema before provider initializes.

let initProducerMembers (producerTy: ProvidedTypeDefinition) uri languageCode =
    let schema = ProducerDescription.Load(resolveUri uri, languageCode)
    let typesTy = ProvidedTypeDefinition("Types", Some baseTy, HideObjectMethods=true)
    producerTy.AddMember(typesTy)
    schema.Services
    |> List.map (fun svc ->
        let serviceTy = ProvidedTypeDefinition(svc.Name, Some baseTy, HideObjectMethods=true)
        serviceTy.AddMembersDelayed(fun _ ->
            svc.Ports
            |> List.map (fun prt ->
                let portTy = ProvidedTypeDefinition(prt.Name, Some baseTy, HideObjectMethods=true)
                let ctr = ProvidedConstructor([])
                ctr.InvokeCode <- fun _ -> <@@ obj() @@>
                portTy.AddMember(ctr)
                portTy)
        )
        serviceTy)
    |> producerTy.AddMembers
    ()
