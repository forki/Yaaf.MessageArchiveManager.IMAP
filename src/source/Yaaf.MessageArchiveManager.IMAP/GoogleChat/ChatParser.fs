// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat

open Yaaf.Helper
open Yaaf.Xmpp.MessageArchiveManager
open System.Xml.Linq
open Yaaf.Logging
open FSharpx.Collections

type ChatMessage = 
    { /// The Target of the message
      To : string
      /// The author of the message
      From : string
      /// Set to -1 to tell the parser to not set this attribute
      SequenceNo : int
      /// Set to -1 to tell the parser to not set this attribute
      LogSource : int
      /// Set to 0 to tell the parser to not set this attribute
      Cid : uint64
      /// A unix Timestamp in ms, set to int64.MinValue to tell the parser to not set this attribute
      TimestampServer : int64
      /// The Chat Message
      Body : string
      /// A unix Timestamp in ms
      TimestampReceived : int64
      CliAsDefault : bool }

type Conversation = 
    { Messages : ChatMessage seq
      // NOTE: Extension to the google format to save the version of the collection
      Version : int
      // Note: Extension to "remember" deleted conversation (note that the Messages Collection MUST be empty when this is true) 
      IsRemoved : bool }

type IChatParser = 
    abstract IsGoogleChat : XElement -> bool
    abstract ParseXml : XElement -> Conversation
    abstract WriteXml : Conversation -> XElement

exception ChatParserException of string

module ChatParser = 
    module XmlExtensions = 
        exception MyXmlException of string
        
        // Some Helper for XML Parsing
        let getAttributes name (elem : XElement) = elem.Attributes() |> Seq.filter (fun a -> a.Name.LocalName = name)
        let getAttribute name (elem : XElement) = 
            getAttributes name elem |> Seq.exactlyOneExn (lazy MyXmlException(sprintf "%s attribute was not found or was found multiple times" name))
        let tryGetAttribute name (elem : XElement) = getAttributes name elem |> Seq.exactlyOneOrNoneExn (lazy MyXmlException(sprintf "%s attribute was found multiple times" name))
        let getElements name (elem : XElement) = elem.Elements() |> Seq.filter (fun a -> a.Name.LocalName = name)
        let getElement name (elem : XElement) = getElements name elem |> Seq.exactlyOneExn (lazy MyXmlException(sprintf "%s element was not found or was found multiple times" name))
        let getValue (elem : XElement) = elem.Value
        let getAttValue (elem : XAttribute) = elem.Value
    
    // Google does use this in the <time ms=""/> Attribute
    let private unixStart = new System.DateTime(1970, 1, 1, 0, 0, 0)
    let convertDateTimeToUnixTimestamp date = (date - unixStart).Ticks / 10000L
    let convertUnixTimestamptoDateTime (stamp : int64) = unixStart.AddTicks(stamp * 10000L)
    
    open XmlExtensions
    
    module RawParser = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        // Google does use this format in the <x stamp=""/> attribute
        let stampFormat = "yyyyMMdd\\Thh\\:mm\\:ss"
        let parseStamp stamp = System.DateTime.ParseExact(stamp, stampFormat, System.Globalization.CultureInfo.InvariantCulture)
        let toStamp (date : System.DateTime) = date.ToString(stampFormat, System.Globalization.CultureInfo.InvariantCulture)
        
        let convertXmlToChatMessage (chatMessageElem : XElement) = 
            try 
                if chatMessageElem.Name.LocalName <> "message" then 
                    Log.Warn(fun _ -> L "the element \"%s\" has not the expected name \"message\"" chatMessageElem.Name.LocalName)
                    raise <| ChatParserException(sprintf "the given element (%s) is no message element" chatMessageElem.Name.LocalName)
                let isCliDefault = 
                    let ns = chatMessageElem.Name.NamespaceName
                    if ns <> "jabber:client" then 
                        Log.Warn(fun _ -> L "%s is not the expected namespace jabber:client" ns)
                        raise <| ChatParserException(sprintf "invalid namespace %s of message element" ns)
                    chatMessageElem.Attributes()
                    |> Seq.filter (fun a -> a.Value = ns)
                    |> Seq.filter (fun a -> a.IsNamespaceDeclaration)
                    |> Seq.isEmpty
                
                // to
                let toValue = 
                    chatMessageElem
                    |> getAttribute "to"
                    |> getAttValue
                
                // from
                let fromValue = 
                    chatMessageElem
                    |> getAttribute "from"
                    |> getAttValue
                
                // int:sequence-no
                let sequenceNo = 
                    match chatMessageElem |> tryGetAttribute "sequence-no" with
                    | Some v -> 
                        v
                        |> getAttValue
                        |> System.Int32.Parse
                    | None -> -1
                
                // d2p1:log-source (d2p1 = int)
                let logSource = 
                    match chatMessageElem |> tryGetAttribute "log-source" with
                    | Some v -> 
                        v
                        |> getAttValue
                        |> System.Int32.Parse
                    | None -> -1
                
                // int:time-stamp
                let serverTime = 
                    match chatMessageElem |> tryGetAttribute "time-stamp" with
                    | Some v -> 
                        v
                        |> getAttValue
                        |> System.Int64.Parse
                    | None -> System.Int64.MinValue
                
                // int:cid
                let cid = 
                    match chatMessageElem |> tryGetAttribute "cid" with
                    | Some v -> 
                        v
                        |> getAttValue
                        |> System.UInt64.Parse
                    | None -> 0UL
                
                // body element = message
                let bodyElem = chatMessageElem |> getElement "body"
                if bodyElem.Name.NamespaceName <> "jabber:client" then 
                    Log.Warn(fun _ -> L "%s is not the expected namespace jabber:client" bodyElem.Name.NamespaceName)
                    raise <| ChatParserException(sprintf "the given body element has an invalid namespace (%s)" bodyElem.Name.NamespaceName)
                let body = bodyElem |> getValue
                // time element because it is more precise and x is redunant?
                let timeElem = chatMessageElem |> getElement "time"
                if timeElem.Name.NamespaceName <> "google:timestamp" then 
                    Log.Warn(fun _ -> L "%s is not the expected namespace google:timestamp" timeElem.Name.NamespaceName)
                    raise <| ChatParserException(sprintf "the given time element has an invalid namespace (%s)" timeElem.Name.NamespaceName)
                let time = 
                    timeElem
                    |> getAttribute "ms"
                    |> getAttValue
                    |> System.Int64.Parse
                { To = toValue
                  From = fromValue
                  SequenceNo = sequenceNo
                  TimestampServer = serverTime
                  Cid = cid
                  LogSource = logSource
                  Body = body
                  TimestampReceived = time
                  CliAsDefault = isCliDefault }
            with MyXmlException msg | ChatParserException msg -> 
                Log.Err(fun _ -> L "%s. Unable to parse message %O" msg chatMessageElem)
                raise <| ChatParserException msg
        
        /// creates the message element from the given message
        /// we ignore google internals, these can be added after calling this function if required
        let convertChatMessageToXml (msg : ChatMessage) = 
            let cliNs = XNamespace.op_Implicit "jabber:client"
            let intNs = XNamespace.op_Implicit "google:internal"
            let xNs = XNamespace.op_Implicit "jabber:x:delay"
            let timeNs = XNamespace.op_Implicit "google:timestamp"
            
            let elem = 
                new XElement(cliNs + "message", new XAttribute(XName.op_Implicit "to", msg.To), new XAttribute(XName.op_Implicit "from", msg.From), 
                             new XAttribute(XNamespace.Xmlns + "int", intNs), new XElement(cliNs + "body", msg.Body), 
                             new XElement(xNs + "x", 
                                          new XAttribute(XName.op_Implicit "stamp", 
                                                         msg.TimestampReceived
                                                         |> convertUnixTimestamptoDateTime
                                                         |> toStamp)), new XElement(timeNs + "time", new XAttribute(XName.op_Implicit "ms", msg.TimestampReceived)))
            if not msg.CliAsDefault then elem.Add(new XAttribute(XNamespace.Xmlns + "cli", cliNs))
            if msg.Cid <> 0UL then elem.Add(new XAttribute(intNs + "cid", msg.Cid))
            if msg.SequenceNo <> -1 then elem.Add(new XAttribute(intNs + "sequence-no", msg.SequenceNo))
            if msg.LogSource <> -1 then elem.Add(new XAttribute(intNs + "log-source", msg.LogSource))
            if msg.TimestampServer <> System.Int64.MinValue then elem.Add(new XAttribute(intNs + "time-stamp", msg.TimestampServer))
            elem
    
    open RawParser
    
    /// create the conversion chat element
    let convertConversationToXml (col : Conversation) = 
        // For some reason google uses the cli namespace for message from other people
        // But no namespace when the message is from us (well its the same namespace but defaulted)
        let conNs = XNamespace.op_Implicit "google:archive:conversation"
        let xElem = new XElement(conNs + "conversation", new XAttribute(XNamespace.Xmlns + "con", conNs), new XAttribute(XName.Get "conversation_version", col.Version))
        if col.IsRemoved then 
            assert (col.Messages |> Seq.isEmpty)
            xElem.Add(new XAttribute(XName.Get "conversation_isremoved", col.IsRemoved))
        else 
            col.Messages
            |> Seq.map convertChatMessageToXml
            |> Seq.iter (fun elem -> xElem.Add(elem))
        xElem
    
    /// parses all message elements from the given conversion element
    let convertXmlToConversation (maybeChat : XElement) = 
        if maybeChat.Name.LocalName <> "conversation" then 
            Log.Warn(fun _ -> L "%s has not the expected name conversation" maybeChat.Name.LocalName)
            raise <| ChatParserException(sprintf "invalid element %s instead of a conversation element" maybeChat.Name.LocalName)
        if maybeChat.Name.NamespaceName <> "google:archive:conversation" then 
            Log.Warn(fun _ -> L "%s is not the expected namespace google:archive:conversation" maybeChat.Name.NamespaceName)
            raise <| ChatParserException(sprintf "invalid namespace %s on conversation element" maybeChat.Name.NamespaceName)
        let version = 
            match maybeChat |> tryGetAttribute "conversation_version" with
            | Some v -> 
                v
                |> getAttValue
                |> System.Int32.Parse
            | None -> -1
        
        let isRemoved = 
            match maybeChat |> tryGetAttribute "conversation_isremoved" with
            | Some v -> 
                v
                |> getAttValue
                |> System.Boolean.Parse
            | None -> false
        
        { Messages = 
              if isRemoved then Seq.empty
              else 
                  maybeChat.Elements()
                  |> Seq.map (convertXmlToChatMessage)
                  |> Seq.build
          Version = version
          IsRemoved = isRemoved }
    
    let isGoogleChat (maybeChat : XElement) = 
        let res = maybeChat.Name.LocalName = "conversation" && maybeChat.Name.NamespaceName = "google:archive:conversation"
        res
    
    let createParser() = 
        { new IChatParser with
              member x.IsGoogleChat chat = isGoogleChat chat
              member x.WriteXml chat = convertConversationToXml chat
              member x.ParseXml xml = convertXmlToConversation xml }
