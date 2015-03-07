// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------

namespace Test.Yaaf.Xmpp.MessageArchiveManager
open System
open NUnit.Framework
open FsUnit

open Yaaf.Xmpp.MessageArchiveManager.IMAP

module TestHelper =
    // Environment.CurrentDirectory -> bin/Debug dir
    let append s parent = System.IO.Path.Combine(parent, s)

    let currentDir = new System.IO.DirectoryInfo( Environment.CurrentDirectory )
    let solutionDir = 
        if currentDir.Name = "Debug" || currentDir.Name = "Release" then
            currentDir.Parent.Parent.Parent.Parent.FullName
        else // running in the test directory
            currentDir.Parent.Parent.Parent.FullName |> append "src"
    let testProjectsDir = solutionDir |> append "test"
    let projectDir =
        testProjectsDir |> append "Test.Yaaf.MessageArchiveManager.IMAP"
    let testFiles = 
        projectDir |> append "TestFiles"
    let testEmails = 
        testFiles |> append "TestEmails"
    let getEmailPath s = 
        testEmails |> append s
    let testChats = 
        testFiles |> append "TestChats"
    let getChatPath s = 
        testChats |> append s
    let loadEmail s = 
        let path = getEmailPath s
        printfn "loading email from: %s" path
        System.IO.File.ReadAllText path
    
[<TestFixture>]
type Test() = 
    [<Test>]
    member this.TestCase  () =        
        true |> should be True
        "On" |> should equal "On"
        ()

