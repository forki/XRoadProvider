module XRoadProvider.Runtime.Protocol

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq

module MyOption =
    let defaultValue d o = o |> Option.fold (fun _ x -> x) d
    let defaultWith f o = match o with Some(o) -> o | None -> f()

[<RequireQualifiedAccessAttribute>]
module internal XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xop = "http://www.w3.org/2004/08/xop/include"
    let [<Literal>] XRoad20 = "http://x-tee.riik.ee/xsd/xtee.xsd"
    let [<Literal>] XRoad30 = "http://x-rd.net/xsd/xroad.xsd"
    let [<Literal>] XRoad31Ee = "http://x-road.ee/xsd/x-road.xsd"
    let [<Literal>] XRoad31Eu = "http://x-road.eu/xsd/x-road.xsd"
    let [<Literal>] XRoad40 = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoad40Id = "http://x-road.eu/xsd/identifiers"
    let [<Literal>] XRoad40Repr = "http://x-road.eu/xsd/representation.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"

    /// Defines namespaces which are handled separately (not generated).
    let predefined =
        [ Http; Mime; Soap; SoapEnc; SoapEnv; Wsdl; Xmime; Xml; Xmlns; Xop; Xsd; Xsi; XRoad40; XRoad40Id; XRoad40Repr ]

[<AutoOpen>]
module internal XRoadProtocolExtensions =
    open XRoadProvider.Runtime.Attributes

    let protocolPrefix = function
        | XRoadProtocol.Version20 -> "xtee"
        | XRoadProtocol.Version30
        | XRoadProtocol.Version31Ee
        | XRoadProtocol.Version31Eu
        | XRoadProtocol.Version40 -> "xrd"
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

    let protocolNamespace = function
        | XRoadProtocol.Version20 -> XmlNamespace.XRoad20
        | XRoadProtocol.Version30 -> XmlNamespace.XRoad30
        | XRoadProtocol.Version31Ee -> XmlNamespace.XRoad31Ee
        | XRoadProtocol.Version31Eu -> XmlNamespace.XRoad31Eu
        | XRoadProtocol.Version40 -> XmlNamespace.XRoad40
        | x -> failwithf "Invalid XRoadProtocol value `%A`" x

module XRoadHelper =
    let getUUID () = Guid.NewGuid().ToString()

    let getSystemTypeName = function
        | "NodaTime.LocalDate" -> Some(XmlQualifiedName("date", XmlNamespace.Xsd))
        | "NodaTime.LocalDateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
        | "System.String" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
        | "System.Boolean" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
        | "System.Decimal" -> Some(XmlQualifiedName("decimal", XmlNamespace.Xsd))
        | "System.Double" -> Some(XmlQualifiedName("double", XmlNamespace.Xsd))
        | "System.Float" -> Some(XmlQualifiedName("float", XmlNamespace.Xsd))
        | "System.Int32" -> Some(XmlQualifiedName("int", XmlNamespace.Xsd))
        | "System.Numerics.BigInteger" -> Some(XmlQualifiedName("integer", XmlNamespace.Xsd))
        | "System.Int64" -> Some(XmlQualifiedName("long", XmlNamespace.Xsd))
        | _ -> None

type internal ContentType =
    | FileStorage of FileInfo
    | Data of byte[]

type public ContentEncoding =
    | Binary = 0
    | Base64 = 1

[<AbstractClass; AllowNullLiteral>]
type public AbstractXRoadHeader() =
    /// Unresolved header elements.
    member val Unresolved = List<XElement>() with get, set

/// Combines X-Road SOAP headers for RPC style messages.
[<AllowNullLiteral>]
type public XRoadRpcHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadRpcHeader) as this = XRoadRpcHeader() then
        if not (other |> isNull) then
            this.Allasutus <- other.Allasutus
            this.Amet <- other.Amet
            this.Ametnik <- other.Ametnik
            this.AmetnikNimi <- other.AmetnikNimi
            this.Andmekogu <- other.Andmekogu
            this.Asutus <- other.Asutus
            this.Asynkroonne <- other.Asynkroonne
            this.Autentija <- other.Autentija
            this.Id <- other.Id
            this.Isikukood <- other.Isikukood
            this.Makstud <- other.Makstud
            this.Salastada <- other.Salastada
            this.SalastadaSertifikaadiga <- other.SalastadaSertifikaadiga
            this.Salastatud <- other.Salastatud
            this.SalastatudSertifikaadiga <- other.SalastatudSertifikaadiga
            this.Toimik <- other.Toimik
            this.Unresolved <- other.Unresolved
    /// Asutuse DNS-nimi.
    member val Asutus = "" with get, set
    /// Andmekogu DNS-nimi.
    member val Andmekogu = "" with get, set
    /// Teenuse kasutaja isikukood, millele eelneb kahekohaline maa kood. Näiteks EE37702026518.
    member val Isikukood = "" with get, set
    /// Teenuse kasutaja Eesti isikukood (ei ole kasutusel alates versioonist 5.0).
    member val Ametnik = "" with get, set
    /// Teenuse väljakutse nonss (unikaalne identifikaator).
    member val Id = "" with get, set
    /// Teenuse väljakutsega seonduva toimiku number (mittekohustuslik).
    member val Toimik = "" with get, set
    /// Asutuse registrikood, mille nimel teenust kasutatakse (kasutusel juriidilise isiku portaalis).
    member val Allasutus = "" with get, set
    /// Teenuse kasutaja ametikoht.
    member val Amet = "" with get, set
    /// Teenuse kasutaja nimi.
    member val AmetnikNimi = "" with get, set
    /// Teenuse kasutamise asünkroonsus. Kui väärtus on "true", siis sooritab turvaserver päringu asünkroonselt.
    member val Asynkroonne = Unchecked.defaultof<Nullable<bool>> with get, set
    /// Teenuse kasutaja autentimise viis. Võimalikud variandid on: ID - ID-kaardiga autenditud; SERT - muu sertifikaadiga autenditud; PANK - panga kaudu autenditud; PAROOL - kasutajatunnuse ja parooliga autenditud. Autentimise viisi järel võib sulgudes olla täpsustus (näiteks panga kaudu autentimisel panga tunnus infosüsteemis).
    member val Autentija = "" with get, set
    /// Teenuse kasutamise eest makstud summa.
    member val Makstud = "" with get, set
    /// Kui asutusele on X-tee keskuse poolt antud päringute salastamise õigus ja andmekogu on nõus päringut salastama, siis selle elemendi olemasolul päringu päises andmekogu turvaserver krüpteerib päringu logi, kasutades selleks X-tee keskuse salastusvõtit.
    member val Salastada = "" with get, set
    /// Päringu sooritaja ID-kaardi autentimissertifikaat DERkujul base64 kodeerituna. Selle elemendi olemasolu päringu päises väljendab soovi päringu logi salastamiseks asutuse turvaserveris päringu sooritaja ID-kaardi autentimisvõtmega. Seda välja kasutatakse ainult kodaniku päringute portaalis."
    member val SalastadaSertifikaadiga = ""B with get, set
    /// Kui päringu välja päises oli element salastada ja päringulogi salastamine õnnestus, siis vastuse päisesse lisatakse tühi element salastatud.
    member val Salastatud = "" with get, set
    /// Kui päringu päises oli element salastada_sertifikaadiga ja päringulogi salastamine õnnestus, siis vastuse päisesesse lisatakse tühi element salastatud_sertifikaadiga.
    member val SalastatudSertifikaadiga = "" with get, set

/// Combines X-Road SOAP headers for document style messages.
[<AllowNullLiteral>]
type public XRoadDocHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadDocHeader) as this = XRoadDocHeader() then
        if not (other |> isNull) then
            this.Async <- other.Async
            this.Authenticator <- other.Authenticator
            this.Consumer <- other.Consumer
            this.Encrypt <- other.Encrypt
            this.EncryptCert <- other.EncryptCert
            this.Encrypted <- other.Encrypted
            this.EncryptedCert <- other.EncryptedCert
            this.Id <- other.Id
            this.Issue <- other.Issue
            this.Paid <- other.Paid
            this.Position <- other.Position
            this.Producer <- other.Producer
            this.Unit <- other.Unit
            this.Unresolved <- other.Unresolved
            this.UserId <- other.UserId
            this.UserName <- other.UserName
    /// DNS-name of the institution
    member val Consumer = "" with get, set
    /// DNS-name of the database
    member val Producer = "" with get, set
    /// ID code of the person invoking the service, preceded by a two-letter country code. For example: EE37702026518.
    member val UserId = "" with get, set
    /// Service invocation nonce (unique identifier).
    member val Id = "" with get, set
    /// Name of file or document related to the service invocation.
    member val Issue = "" with get, set
    /// Registration code of the institution or its unit on whose behalf the service is used (applied in the legal entity portal).
    member val Unit = "" with get, set
    /// Organizational position or role of the person invoking the service.
    member val Position = "" with get, set
    /// Name of the person invoking the service.
    member val UserName = "" with get, set
    /// Specifies asynchronous service. If the value is "true", then the security server performs the service call asynchronously.
    member val Async = Unchecked.defaultof<Nullable<bool>> with get, set
    /// Authentication method, one of the following: ID-CARD - with a certificate of identity; CERT - with another certificate; EXTERNAL - through a third-party service; PASSWORD - with user ID and a password. Details of the authentication (e.g. the identification of a bank for external authentication) can be given in brackets after the authentication method.
    member val Authenticator = "" with get, set
    /// The amount of money paid for invoking the service.
    member val Paid = "" with get, set
    /// If an organization has got the right from the X-Road Center to hide queries, with the database agreeing to hide the query, the occurrence of this tag in the query header makes the database security server to encrypt the query log, using the encryption key of the X-Road Center.
    member val Encrypt = "" with get, set
    /// Authentication certificate of the query invokers ID Card, in the base64-encoded DER format. Occurrence of this tag in the query header represents the wish to encrypt the query log in the organizations security server, using authentication key of the query invokers ID Card. This field is used in the Citizen Query Portal only.
    member val EncryptCert = ""B with get, set
    /// If the query header contains the encrypt tag and the query log as been successfully encrypted, an empty encrypted tag will be inserted in the reply header.
    member val Encrypted = "" with get, set
    /// If the query header contains the encryptedCert tag and the query log has been successfully encrypted, an empty encryptedCert tag will accordingly be inserted in the reply header.
    member val EncryptedCert = "" with get, set

/// Represents identifiers that can be used by the service clients, namely X-Road members and subsystems.
[<AllowNullLiteral>]
type public XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) =
    new () = XRoadMemberIdentifier("", "", "", "")
    new (xRoadInstance, memberClass, memberCode) = XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, "")
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = xRoadInstance with get, set
    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = memberClass with get, set
    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = memberCode with get, set
    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = subsystemCode with get, set

/// Represents identifiers of services.
[<AllowNullLiteral>]
type public XRoadServiceIdentifier() =
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = "" with get, set
    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = "" with get, set
    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = "" with get, set
    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = "" with get, set
    /// The service code is chosen by the service provider.
    member val ServiceCode = "" with get, set
    /// Version is optional and can be used to distinguish between technically incompatible versions of the same basic service.
    member val ServiceVersion = "" with get, set

/// Represents identifiers of central services.
[<AllowNullLiteral>]
type public XRoadCentralServiceIdentifier() =
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = "" with get, set
    /// The service code is chosen by the service provider.
    member val ServiceCode = "" with get, set

/// Combines X-Road SOAP headers for X-Road v6.
[<AllowNullLiteral>]
type public XRoadHeader() =
    inherit AbstractXRoadHeader()
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadHeader) as this = XRoadHeader() then
        if not (other |> isNull) then
            this.CentralService <- other.CentralService
            this.Client <- other.Client
            this.Id <- other.Id
            this.Issue <- other.Issue
            this.Producer <- other.Producer
            this.ProtocolVersion <- other.ProtocolVersion
            this.RequestHash <- other.RequestHash
            this.RequestHashAlgorithm <- other.RequestHashAlgorithm
            this.Unresolved <- other.Unresolved
            this.UserId <- other.UserId
    /// Identifies a service client – an entity that initiates the service call.
    member val Client = XRoadMemberIdentifier() with get, set
    /// Identifies the service that is invoked by the request.
    member val Producer = XRoadMemberIdentifier() with get, set
    /// Identifies the central service that is invoked by the request.
    member val CentralService = XRoadCentralServiceIdentifier() with get, set
    /// Unique identifier for this message. The recommended form of message ID is UUID.
    member val Id = "" with get, set
    /// User whose action initiated the request. The user ID should be prefixed with two-letter ISO country code (e.g., EE12345678901).
    member val UserId = "" with get, set
    /// For responses, this field contains a Base64 encoded hash of the request SOAP message.
    /// This field is automatically filled in by the service provider's security server.
    member val RequestHash = "" with get, set
    /// Identifies the hash algorithm that was used to calculate the value of the requestHash field.
    /// The algorithms are specified as URIs listed in the XML-DSIG specification [DSIG].
    member val RequestHashAlgorithm = "" with get, set
    /// Identifies received application, issue or document that was the cause of the service request.
    /// This field may be used by the client information system to connect service requests (and responses) to working procedures.
    member val Issue = "" with get, set
    /// X-Road message protocol version. The value of this field MUST be 4.0
    member val ProtocolVersion = "" with get, set

[<AllowNullLiteral>]
type public BinaryContent internal (contentID: string, content: ContentType) =
    member val ContentEncoding = ContentEncoding.Binary with get, set
    member __.ContentID
        with get() =
            match contentID with
            | null | "" -> XRoadHelper.getUUID()
            | _ -> contentID
    member __.OpenStream() : Stream =
        match content with
        | FileStorage(file) -> upcast file.OpenRead()
        | Data(data) -> upcast new MemoryStream(data)
    member __.GetBytes() =
        match content with
        | FileStorage(file) -> File.ReadAllBytes(file.FullName)
        | Data(data) -> data
    static member Create(file) = BinaryContent("", FileStorage(file))
    static member Create(contentID, file) = BinaryContent(contentID, FileStorage(file))
    static member Create(data) = BinaryContent("", Data(data))
    static member Create(contentID, data) = BinaryContent(contentID, Data(data))

type XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

[<AllowNullLiteral>]
type SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachments(attachments: IDictionary<_,_>) =
        match attachments with
        | null -> ()
        | _ -> attachments |> Seq.iter (fun kvp -> this.Attachments.Add(kvp.Key, kvp.Value))
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> failwithf "Multipart message doesn't contain content part with ID `%s`." contentID
        else failwithf "Invalid multipart content reference: `%s`." href
