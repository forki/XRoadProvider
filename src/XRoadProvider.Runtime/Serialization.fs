namespace XRoadProvider.Runtime

open Common.Logging
open Emitter
open FSharp.Core
open Protocol
open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Xml
open System.Xml.Linq
open XRoadProvider.Runtime.Attributes

module Stream =
    let toString (stream: Stream) =
        stream.Position <- 0L
        let reader = new StreamReader(stream)
        reader.ReadToEnd()

module private Response =
    type XmlReader with
        member this.MoveToElement(depth, name, ns) =
            while this.Depth < depth do this.Read() |> ignore
            let isElement () = this.Depth = depth && this.NodeType = XmlNodeType.Element && (name |> isNull || (this.LocalName = name && this.NamespaceURI = ns))
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

module internal MultipartMessage =
    open System.Net
    open System.Text

    type private ChunkState = Limit | NewLine | EndOfStream

    type private PeekStream(stream: Stream) =
        let mutable borrow = None : int option
        member __.Read() =
            match borrow with
            | Some(x) ->
                borrow <- None
                x
            | None -> stream.ReadByte()
        member __.Peek() =
            match borrow with
            | None ->
                let x = stream.ReadByte()
                borrow <- Some(x)
                x
            | Some(x) -> x
        member __.Flush() = stream.Flush()
        interface IDisposable with
            member __.Dispose() =
                stream.Dispose()

    let private getBoundaryMarker (response: WebResponse) =
        let parseMultipartContentType (contentType: string) =
            let parts = contentType.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofArray
                        |> List.map (fun x -> x.Trim())
            match parts with
            | "multipart/related" :: parts ->
                parts |> List.tryFind (fun x -> x.StartsWith("boundary="))
                      |> Option.map (fun x -> x.Substring(9).Trim('"'))
            | _ -> None
        response
        |> Option.ofObj
        |> Option.map (fun r -> r.ContentType)
        |> Option.bind (parseMultipartContentType)

    let [<Literal>] private CHUNK_SIZE = 4096
    let [<Literal>] private CR = 13
    let [<Literal>] private LF = 10

    let private readChunkOrLine (buffer: byte []) (stream: PeekStream) =
        let rec addByte pos =
            if pos >= CHUNK_SIZE then (Limit, pos)
            else
                match stream.Read() with
                | -1 -> (EndOfStream, pos)
                | byt ->
                    if byt = CR && stream.Peek() = LF then
                        stream.Read() |> ignore
                        (NewLine, pos)
                    else
                        buffer.[pos] <- Convert.ToByte(byt)
                        addByte (pos + 1)
        let result = addByte 0
        stream.Flush()
        result

    let private readLine stream =
        let mutable line: byte[] = [||]
        let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            Array.Copy(buffer, line, chunkSize)
            match state with
            | Limit -> readChunk()
            | EndOfStream
            | NewLine -> ()
        readChunk()
        line

    let private extractMultipartContentHeaders (stream: PeekStream) =
        let rec getHeaders () = seq {
            match Encoding.ASCII.GetString(stream |> readLine).Trim() with
            | null | "" -> ()
            | line ->
                let (key, value) =
                    match line.Split([| ':' |], 2) with
                    | [| name |] -> (name, "")
                    | [| name; content |] -> (name, content)
                    | _ -> failwith "never"
                yield (key.Trim().ToLower(), value.Trim())
                yield! getHeaders() }
        getHeaders() |> Map.ofSeq

    let private base64Decoder (encoding: Encoding) (encodedBytes: byte []) =
        match encodedBytes with
        | null | [| |] -> [| |]
        | _ ->
            let chars = encoding.GetChars(encodedBytes)
            Convert.FromBase64CharArray(chars, 0, chars.Length)

    let private getDecoder (contentEncoding: string) =
        match contentEncoding.ToLower() with
        | "base64" -> Some(base64Decoder)
        | "quoted-printable" | "7bit" | "8bit" | "binary" -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let private startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] <> buffer.[i] then false else
            if i = 0 then true else compare (i - 1)
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let internal read (response: WebResponse) : Stream * BinaryContent list =
        match response |> getBoundaryMarker with
        | Some(boundaryMarker) ->
            use stream = new PeekStream(response.GetResponseStream())
            let contents = List<string option * MemoryStream>()
            let isContentMarker = startsWith (Encoding.ASCII.GetBytes (sprintf "--%s" boundaryMarker))
            let isEndMarker = startsWith (Encoding.ASCII.GetBytes (sprintf "--%s--" boundaryMarker))
            let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
            let rec copyChunk addNewLine encoding (decoder: (Encoding -> byte[] -> byte[]) option) (contentStream: Stream) =
                let (state,size) = stream |> readChunkOrLine buffer
                if buffer |> isEndMarker then false
                elif buffer |> isContentMarker then true
                elif state = EndOfStream then failwith "Unexpected end of multipart stream."
                else
                    if decoder.IsNone && addNewLine then contentStream.Write([| 13uy; 10uy |], 0, 2)
                    let (decodedBuffer,size) = decoder |> Option.fold (fun (buf,_) func -> let buf = buf |> func encoding in (buf,buf.Length)) (buffer,size)
                    contentStream.Write(decodedBuffer, 0, size)
                    match state with EndOfStream -> false | _ -> copyChunk (state = NewLine) encoding decoder contentStream
            let rec parseNextContentPart () =
                let headers = stream |> extractMultipartContentHeaders
                let contentId = headers |> Map.tryFind("content-id") |> Option.map (fun x -> x.Trim().Trim('<', '>'))
                let decoder = headers |> Map.tryFind("content-transfer-encoding") |> Option.bind (getDecoder)
                let contentStream = new MemoryStream()
                contents.Add(contentId, contentStream)
                if copyChunk false Encoding.UTF8 decoder contentStream |> not then ()
                else parseNextContentPart() 
            let rec parseContent () =
                let line = stream |> readLine
                if line |> isEndMarker then ()
                elif line |> isContentMarker then parseNextContentPart()
                else parseContent()
            parseContent()
            match contents |> Seq.toList with
            | (_,content)::attachments ->
                (upcast content, attachments
                                 |> List.map (fun (name,stream) ->
                                    use stream = stream
                                    stream.Position <- 0L
                                    BinaryContent.Create(name.Value, stream.ToArray())))
            | _ -> failwith "empty multipart content"
        | None ->
            use stream = response.GetResponseStream()
            let content = new MemoryStream()
            stream.CopyTo(content)
            (upcast content, [])

open Protocol
open Response
open System.Xml.XPath

type XRoadResponse(response: WebResponse, methodMap: MethodMap) =
    let log = LogManager.GetLogger()

    let checkXRoadFault (stream: Stream) =
        let faultPath = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*"
        let xpath =
            match methodMap.Protocol with
            | XRoadProtocol.Version40 -> faultPath
            | XRoadProtocol.Version20 -> sprintf "%s/keha" faultPath
            | _ -> sprintf "%s/response" faultPath
        stream.Position <- 0L
        use reader = XmlReader.Create(stream)
        let doc = XPathDocument(reader)
        let nav = doc.CreateNavigator()
        match nav.SelectSingleNode(sprintf "%s[faultCode|faultString]" xpath) with
        | null -> ()
        | node ->
            let faultCode = node.SelectSingleNode("./faultCode")
            let faultString = node.SelectSingleNode("./faultString")
            let nodeToString = Option.ofObj >> Option.map (fun x -> (x: XPathNavigator).InnerXml) >> MyOption.defaultValue ""
            raise(XRoadFault(faultCode |> nodeToString, faultString |> nodeToString))

    member __.RetrieveMessage() =
        let attachments = Dictionary<string, BinaryContent>()
        use stream =
            let stream, atts = response |> MultipartMessage.read
            atts |> List.iter (fun content -> attachments.Add(content.ContentID, content))
            stream
        if log.IsTraceEnabled then
            log.Trace(stream |> Stream.toString)
        stream |> checkXRoadFault
        stream.Position <- 0L
        use reader = XmlReader.Create(stream)
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        let context = SerializerContext()
        context.AddAttachments(attachments)
        if not (reader.MoveToElement(2, null, null)) then
            failwith "Soap message has empty payload in response."
        // TODO : validate response wrapper element
        match reader.LocalName, reader.NamespaceURI with
        | "Fault", XmlNamespace.SoapEnv -> failwithf "Request resulted an error: %s" (reader.ReadInnerXml())
        | _ -> methodMap.Deserializer.Invoke(reader, context)

    interface IDisposable with
        member __.Dispose() =
            (response :> IDisposable).Dispose()

type XRoadStreamWriter() =
    class
    end

type XRoadStreamReader() =
    class
    end

type XRoadRequest(producerUri: string, methodMap: MethodMap) =
    let log = LogManager.GetLogger()

    let request = WebRequest.Create(producerUri, Method="POST", ContentType="text/xml; charset=utf-8")
    do request.Headers.Set("SOAPAction", "")
    
    let addNamespace =
        let mutable i = 0
        (fun ns (writer: XmlWriter) ->
            if writer.LookupPrefix(ns) |> isNull then
                i <- i + 1
                writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))

    let writeContent (stream: Stream) (content: Stream) =
        let buffer = Array.create 1000 0uy
        let rec writeChunk() =
            let bytesRead = content.Read(buffer, 0, 1000)
            stream.Write(buffer, 0, bytesRead)
            match bytesRead with 1000 -> writeChunk() | _ -> ()
        content.Position <- 0L
        writeChunk()

    let serializeMultipartMessage (attachments: Dictionary<string,BinaryContent>) (serializeContent: Stream -> unit) =
        use stream = request.GetRequestStream()
        if attachments.Count > 0 then
            use writer = new StreamWriter(stream, NewLine = "\r\n")
            let boundaryMarker = Guid.NewGuid().ToString()
            request.ContentType <- sprintf @"multipart/related; type=""text/xml""; boundary=""%s""" boundaryMarker
            request.Headers.Add("MIME-Version", "1.0")
            writer.WriteLine()
            writer.WriteLine("--{0}", boundaryMarker)
            writer.WriteLine("Content-Type: text/xml; charset=UTF-8")
            writer.WriteLine("Content-Transfer-Encoding: 8bit")
            writer.WriteLine("Content-ID: <XML-{0}>", boundaryMarker)
            writer.WriteLine()
            writer.Flush()
            stream |> serializeContent
            attachments |> Seq.iter (fun kvp ->
                writer.WriteLine()
                writer.WriteLine("--{0}", boundaryMarker)
                writer.WriteLine("Content-Disposition: attachment; filename=notAnswering")
                writer.WriteLine("Content-Type: application/octet-stream")
                writer.WriteLine("Content-Transfer-Encoding: binary")
                writer.WriteLine("Content-ID: <{0}>", kvp.Key)
                writer.WriteLine()
                writer.Flush()
                use contentStream = kvp.Value.OpenStream()
                writeContent stream contentStream
                writer.WriteLine())
            writer.WriteLine("--{0}--", boundaryMarker)
        else stream |> serializeContent

    let serializeMessage (content: Stream) (attachments: Dictionary<string,BinaryContent>) =
        serializeMultipartMessage attachments (fun s -> writeContent s content)

    let writeIdHeader value ns req (writer: XmlWriter) =
        if req |> Array.exists ((=) "id") || value |> String.IsNullOrEmpty |> not then
            writer.WriteStartElement("id", ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            writer.WriteValue(if String.IsNullOrWhiteSpace(value) then XRoadHelper.getUUID() else value)
            writer.WriteEndElement()

    let writeStringHeader req ns (writer: XmlWriter) value name =
        if req |> Array.exists ((=) name) || value |> String.IsNullOrEmpty |> not then
            writer.WriteStartElement(name, ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if String.IsNullOrEmpty(value) |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeBoolHeader (value: Nullable<bool>) name ns req (writer: XmlWriter) =
        if req |> Array.exists ((=) name) || value.HasValue then
            writer.WriteStartElement(name, ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("boolean", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if value.HasValue then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeBase64Header (value: byte[]) name ns req (writer: XmlWriter) =
        let value = value |> Option.ofObj |> MyOption.defaultWith (fun _ -> [||])
        if req |> Array.exists ((=) name) || value |> Array.isEmpty |> not then
            writer.WriteStartElement(name, ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("base64", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if value |> Array.isEmpty |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeClientHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "client") || not (value |> isNull) then
            writer.WriteStartElement("client", XmlNamespace.XRoad40)
            if not (value |> isNull) then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoad40Id)
                writer.WriteValue(if String.IsNullOrWhiteSpace(value.SubsystemCode) then "MEMBER" else "SUBSYSTEM")
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoad40Id)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
            writer.WriteEndElement()

    let getServiceName producerName =
        let serviceName = match methodMap.ServiceVersion with
                          | Some(version) -> sprintf "%s.%s" methodMap.ServiceCode version
                          | None -> methodMap.ServiceCode
        if producerName |> String.IsNullOrEmpty then serviceName
        else sprintf "%s.%s" producerName serviceName

    let writeServiceHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "service") || not (value |> isNull) then
            writer.WriteStartElement("service", XmlNamespace.XRoad40)
            if not (value |> isNull) then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoad40Id)
                writer.WriteValue("SERVICE")
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoad40Id)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
                writer.WriteStartElement("serviceCode", XmlNamespace.XRoad40Id)
                if not <| String.IsNullOrEmpty(methodMap.ServiceCode) then
                    writer.WriteValue(methodMap.ServiceCode)
                writer.WriteEndElement()
                match methodMap.ServiceVersion with
                | Some(version) ->
                    writer.WriteStartElement("serviceVersion", XmlNamespace.XRoad40Id)
                    writer.WriteValue(version)
                    writer.WriteEndElement()
                | None -> ()
            writer.WriteEndElement()

    let writeXRoadHeader (header: AbstractXRoadHeader) (writer: XmlWriter) =
        methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
        let headerNamespace = protocolNamespace methodMap.Protocol
        let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(headerNamespace) with true, xs -> xs | _ -> [||]
        let writeStringHeader' = writeStringHeader requiredHeaders headerNamespace writer
        match header with
        | :? XRoadRpcHeader as header ->
            writeStringHeader' header.Asutus "asutus"
            writeStringHeader' header.Andmekogu "andmekogu"
            writeStringHeader' header.Isikukood "isikukood"
            writeStringHeader' header.Ametnik "ametnik"
            writer |> writeIdHeader header.Id headerNamespace requiredHeaders
            writeStringHeader' (getServiceName header.Andmekogu) "nimi"
            writeStringHeader' header.Toimik "toimik"
            writeStringHeader' header.Allasutus "allasutus"
            writeStringHeader' header.Amet "amet"
            writeStringHeader' header.AmetnikNimi "ametniknimi"
            writer |> writeBoolHeader header.Asynkroonne "asynkroonne" headerNamespace requiredHeaders
            writeStringHeader' header.Autentija "autentija"
            writeStringHeader' header.Makstud "makstud"
            writeStringHeader' header.Salastada "salastada"
            writer |> writeBase64Header header.SalastadaSertifikaadiga "salastada_sertifikaadiga" headerNamespace requiredHeaders
            writeStringHeader' header.Salastatud "salastatud"
            writeStringHeader' header.SalastatudSertifikaadiga "salastatud_sertifikaadiga"
        | :? XRoadDocHeader as header ->
            writeStringHeader' header.Consumer "consumer"
            writeStringHeader' header.Producer "producer"
            writeStringHeader' header.UserId "userId"
            writer |> writeIdHeader header.Id headerNamespace requiredHeaders
            writeStringHeader' (getServiceName header.Producer) "service"
            writeStringHeader' header.Issue "issue"
            writeStringHeader' header.Unit "unit"
            writeStringHeader' header.Position "position"
            writeStringHeader' header.UserName "userName"
            writer |> writeBoolHeader header.Async "async" headerNamespace requiredHeaders
            writeStringHeader' header.Authenticator "authenticator"
            writeStringHeader' header.Paid "paid"
            writeStringHeader' header.Encrypt "encrypt"
            writer |> writeBase64Header header.EncryptCert "encryptCert" headerNamespace requiredHeaders
            writeStringHeader' header.Encrypted "encrypted"
            writeStringHeader' header.EncryptedCert "encryptedCert"
        | :? XRoadHeader as header ->
            if writer.LookupPrefix(XmlNamespace.XRoad40Id) |> isNull then
                writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoad40Id)
            writer |> writeClientHeader header.Client requiredHeaders
            writer |> writeServiceHeader header.Producer requiredHeaders
            writer |> writeIdHeader header.Id headerNamespace requiredHeaders
            writeStringHeader' header.UserId "userId"
            writeStringHeader' header.Issue "issue"
            writeStringHeader' header.ProtocolVersion "protocolVersion"
        | _ -> failwithf "Unexpected X-Road header type `%s`." (header.GetType().FullName)
        header.Unresolved |> Seq.iter (fun e -> e.WriteTo(writer))

    member __.SendMessage(header: AbstractXRoadHeader, args: obj[]) =
        use content = new MemoryStream()
        use sw = new StreamWriter(content)
        let context = SerializerContext(IsMultipart = methodMap.Request.IsMultipart)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", protocolPrefix methodMap.Protocol, XmlNamespace.Xmlns, protocolNamespace methodMap.Protocol)
        methodMap.Namespaces |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))
        methodMap.Request.Accessor |> Option.iter (fun acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace))
        if methodMap.Request.IsEncoded then
            writer.WriteAttributeString("xmlns", "xsd", XmlNamespace.Xmlns, XmlNamespace.Xsd)
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        writer |> writeXRoadHeader header
        writer.WriteEndElement()
        
        writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
        methodMap.Serializer.Invoke(writer, args, context)
        writer.WriteEndElement()

        writer.WriteEndDocument()
        writer.Flush()
        if log.IsTraceEnabled then
            log.Trace(content |> Stream.toString)
        serializeMessage content context.Attachments
    member __.GetResponse(methodMap: MethodMap) =
        new XRoadResponse(request.GetResponse(), methodMap)

type public XRoadUtil =
    static member MakeServiceCall(serviceType: Type, methodName: string, producerUri: string, header: AbstractXRoadHeader, args: obj[]) =
        let serviceMethod = serviceType.GetMethod(methodName)
        let serviceMethodMap = getMethodMap serviceMethod 
        let request = XRoadRequest(producerUri, serviceMethodMap)
        request.SendMessage(header, args)
        use response = request.GetResponse(serviceMethodMap)
        response.RetrieveMessage()
