// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------

namespace Test.Yaaf.Xmpp.MessageArchiveManager.IMAP

open System
open NUnit.Framework
open FsUnit

open Test.Yaaf.Xmpp.MessageArchiveManager.TestHelper

open Yaaf.Xmpp.MessageArchiveManager.IMAP
open Yaaf.Xmpp.MessageArchiveManager.IMAP.ActiveUpImap4

open Yaaf.Logging

[<TestFixture>]
type ``Given the parseMessage method``() = 
    let T = Log.EmptyTracer
    
    let messageFromMail m = 
        let raw = loadEmail m
        let msg = ActiveUp.Net.Mail.Parser.ParseMessage(raw)
        parseMessage msg 
    
    
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    [<TestCase(11)>]
    [<TestCase(12)>]
    [<Test>]
    [<Description("Test if we can parse a simple Message without Chat")>]
    member this.
        ``Test if we can parse a simple Message without Chat``  (num:int) =   
        let parsed = messageFromMail ("simpleTest_" + num.ToString()) num
        let recieveDate = parsed.Date
        // All emails are within this range
        recieveDate |> should be (greaterThan (new DateTime(2013, 02, 09)))
        recieveDate |> should be (lessThan (new DateTime(2013, 02, 11)))
        
        // No Xml in this Email
        parsed.XmlContent.IsNone |> should be True
        
        // Only very simple messages
        parsed.TextContent.Length |> should be (lessThan 100)
        
        
        ()
    [<Test>]
    member this.
        ``Test if we can parse the Mesh Message without Chat``  () =
        let parsed = messageFromMail "meshRawMail" 1
        let recieveDate = parsed.Date
        recieveDate |> should not' (equal (new DateTime()))
        parsed.MessageID |> should be (equal "em.7c371959.20130206152043.2.349.7375999.11269902@email.microsoft.com")
        ()
        
    [<Test>]
    member this.
        ``Test if we can parse a simple Message with Chat``  () =
        let parsed = messageFromMail "simpleGoogleChat" 1
        let recieveDate = parsed.Date
        recieveDate |> should not' (equal (new DateTime()))
        parsed.MessageID |> should be (equal "17638656.601295.1357519049616.chat@gmail.com")
        
        parsed.XmlContent.IsSome |> should be True
        ()  
    