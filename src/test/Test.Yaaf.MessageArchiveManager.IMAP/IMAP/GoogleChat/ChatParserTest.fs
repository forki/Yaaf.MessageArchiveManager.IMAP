// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManagerTest.IMAP.GoogleChat

open System
open NUnit.Framework
open FsUnit
open Test.Yaaf.Xmpp.MessageArchiveManager.TestHelper
open Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat
open Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat.ChatParser
open Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat.ChatParser.RawParser
open System.Xml.Linq

open Yaaf.Logging

[<TestFixture>]
type ``Given the ChatParser``() = 
    let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
    let advancedResult =
        @"<cli:message to=""matthi.d@googlemail.com"" from=""root@yaaf.no-ip.org"" xmlns:int=""google:internal"" xmlns:cli=""jabber:client"" int:log-source=""2"">
    <cli:body>lol</cli:body>
    <x stamp=""20130107T12:37:29"" xmlns=""jabber:x:delay"" />
    <time ms=""1357519049615"" xmlns=""google:timestamp"" />
</cli:message>"
    
    [<Test>]
    member this.
        ``If we can parse the simple xmlExample`` () = 
        let parsedMessages = convertXmlToConversation (XElement.Parse xmlExample)
        
        parsedMessages.Messages |> Seq.length |> should be (equal 1)
        
        let msg = parsedMessages.Messages |> Seq.exactlyOne
        msg.Body |> should be (equal "lol")
        msg.From |> should be (equal "root@yaaf.no-ip.org")
        msg.To |> should be (equal "matthi.d@googlemail.com")
        msg.TimestampReceived |> should be (equal 1357519049615L)                
        
    [<Test>]
    member this.
        ``If we can build the simple xmlExample`` () = 
        let xml = XElement.Parse xmlExample
        let parsedMessage = (convertXmlToConversation xml).Messages  |> Seq.exactlyOne
        
        parsedMessage.CliAsDefault |> should be False
        
        let opts = SaveOptions.DisableFormatting ||| SaveOptions.OmitDuplicateNamespaces
        
        let advancedXml = parsedMessage |> convertChatMessageToXml
        let expectAdvanced = XElement.Parse advancedResult
        advancedXml.ToString(opts) |> should be (equal (expectAdvanced.ToString(opts)))

    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "msg", 1234125120000L, true)>]    
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "msg", 1234125120000L, false)>]
    [<TestCase("q78905z$%&/(", "asdas@asd <>23", 123, 12315, 239456728394756UL, 234123532, "My Message 2983471234a§$%&/()=", 1234125120000L, false)>]
    [<TestCase("q78905z$%&/(", "asdas@asd <>23", 123, 12315, 239456728394756UL, 234123532, "My Message 2983471234a§$%&/()=", 1234125120000L, true)>]
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "Really H  \"}{\"<xml*'#+=<<<!--", 1234125120000L, false)>]
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "<![CDATA[ blub", 1234125120000L, false)>]
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "/> /> >>", 1234125120000L, false)>]
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "]]> ]]> asd", 1234125120000L, false)>]
    [<TestCase("to", "from", -1, -1, 0UL, System.Int64.MinValue, "Really H  \"}{\"<xml*'#+=<<<!--", 1234125120000L, false)>]
    [<Test>]
    member this.
        ``If we preserve RAW Chat messages`` (toString, from, sequenceNo, logSource, cid, timestampServer, body, timestampReceived, cliAsDefault) = 
        let expectMsg =
            {
                /// The Target of the message
                To = toString
                /// The author of the message
                From = from
                /// Set to -1 to tell the parser to not set this attribute
                SequenceNo = sequenceNo
                /// Set to -1 to tell the parser to not set this attribute
                LogSource = logSource
                /// Set to 0 to tell the parser to not set this attribute
                Cid = cid
                /// A unix Timestamp in ms, set to int64.MinValue to tell the parser to not set this attribute
                TimestampServer = timestampServer
                /// The Chat Message
                Body = body
                /// A unix Timestamp in ms
                TimestampReceived = timestampReceived
                CliAsDefault = cliAsDefault
            }
        let xml = convertChatMessageToXml expectMsg
        let actual = convertXmlToChatMessage xml
        
        actual.To |> should be (equal expectMsg.To)
        actual.From |> should be (equal expectMsg.From)
        actual.SequenceNo |> should be (equal expectMsg.SequenceNo)
        actual.LogSource |> should be (equal expectMsg.LogSource)
        actual.Cid |> should be (equal expectMsg.Cid)
        actual.TimestampServer |> should be (equal expectMsg.TimestampServer)
        actual.Body |> should be (equal expectMsg.Body)
        actual.TimestampReceived |> should be (equal expectMsg.TimestampReceived)
        actual.CliAsDefault |> should be (equal expectMsg.CliAsDefault)
    
    [<Test>]
    member this.
        ``If we fail with the right exception when the message name is invalid`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:msg to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:msg>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    [<Test>]
    member this.
        ``If we fail with the right exception when the conversation name is invalid`` () = 
        let xmlExample = 
            "<con:conv xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conv>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    [<Test>]
    member this.
        ``If we fail with the right exception when the conversation namespace is invalid`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"sd\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    [<Test>]
    member this.
        ``If we fail with the right exception when the conversation namespace is invalid 2`` () = 
        let xmlExample = 
            "<conversation>\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    [<Test>]
    member this.
        ``If we fail with the right exception when the message namespace is invalid`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"invalid\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    [<Test>]
    member this.
        ``If we fail with the right exception when the to attribute is missing`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>

    [<Test>]
    member this.
        ``If we fail with the right exception when the from attribute is missing`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>

    [<Test>]
    member this.
        ``If we fail with the right exception when the body element is missing`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>

    [<Test>]
    member this.
        ``If we fail with the right exception when the body element has an invalid namespace`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <body>lol</body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>

    [<Test>]
    member this.
        ``If we fail with the right exception when the timestamp element is missing`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    
    [<Test>]
    member this.
        ``If we fail with the right exception when the timestamp element has no attribut`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time xmlns=\"google:timestamp\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>
    
    [<Test>]
    member this.
        ``If we fail with the right exception when the timestamp element has the wrong namespace`` () = 
        let xmlExample = 
            "<con:conversation xmlns:con=\"google:archive:conversation\">\n  <cli:message to=\"matthi.d@googlemail.com\" from=\"root@yaaf.no-ip.org\" d2p1:log-source=\"2\" xmlns:cli=\"jabber:client\" xmlns:int=\"google:internal\" xmlns:d2p1=\"google:internal\">\n    <cli:body>lol</cli:body>\n    <x stamp=\"20130107T00:37:29\" xmlns=\"jabber:x:delay\" />\n    <time ms=\"1357519049615\" xmlns=\"asd\" />\n  </cli:message>\n</con:conversation>"
        let xml = XElement.Parse xmlExample
        (fun () -> convertXmlToConversation xml |> ignore) |> should throw typeof<ChatParserException>

        
        
        