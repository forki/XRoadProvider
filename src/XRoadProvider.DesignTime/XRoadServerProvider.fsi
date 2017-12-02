/// Implementations for the type providers that are available for X-Road infrastructure.
namespace ProviderImplementation.ProvidedTypes

/// Erased type provider for acquiring X-Road producers from security server.
type XRoadServerProvider =
    inherit ProviderImplementation.ProvidedTypes.TypeProviderForNamespaces

    /// Initializes new type provider instance
    new: Microsoft.FSharp.Core.CompilerServices.TypeProviderConfig -> XRoadServerProvider
