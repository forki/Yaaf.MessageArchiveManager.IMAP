// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------

namespace Yaaf.Xmpp.MessageArchiveManagerTest.IMAP

open System
open NUnit.Framework
open FsUnit

open Yaaf.FSharp.Control
open System.Threading.Tasks

open Test.Yaaf.Xmpp.MessageArchiveManager.TestHelper
open Yaaf.Xmpp.MessageArchiveManager.IMAP
open Yaaf.Xmpp.MessageArchiveManager.IMAP.QueryParser


type MemoryMessageArchivingStore () = 
    let userPrefStore = new System.Collections.Generic.Dictionary<_,_>()
    let userColStore = new System.Collections.Generic.Dictionary<_, _>()

    interface Yaaf.Xmpp.MessageArchiving.IMessageArchivingStore with
        member x.GetPreferenceStore jid =
            match userPrefStore.TryGetValue jid.BareId with
            | true, v -> Task.FromResult v
            | false, _ -> 
                let newStore = Yaaf.Xmpp.MessageArchiving.MemoryUserPreferenceStore(jid.BareJid) :> Yaaf.Xmpp.MessageArchiving.IUserPreferenceStore
                userPrefStore.Add(jid.BareId, newStore)
                Task.FromResult newStore
        member x.GetArchiveStore jid  =
            let imap =
                match userColStore.TryGetValue jid.BareId with
                | true, v -> v
                | false, _ -> 
                    let wrapper = MemoryImapProcessor.GetImap()
                    let imap = ImapConnection(wrapper, "blub", "blub", "blub")
                    userColStore.Add(jid.BareId, imap)
                    imap
            let user = jid.BareJid
            ImapArchiveManager.convertImapToArchiveManager
                "chats" 
                (GoogleChat.MessageMapper.createMessageMapper user)
                user
                imap
              :> Yaaf.Xmpp.MessageArchiving.IUserArchivingStore
              |> Task.FromResult 

// TODO
//[<TestFixture>]
//type ImapArchiveManagerTest() = 
//    inherit Test.Yaaf.Xmpp.IM.MessageArchivingStoreTest()
//    
//    override x.CreateArchivingStore () = 
//        new  MemoryMessageArchivingStore() :> Yaaf.Xmpp.IM.MessageArchiving.Core.MessageArchivingStore
//
//    [<Test>]
//    member this.``Test if we can build queries with dates`` () = 
//        ()


type YaafImapMessageArchivingStoreTest () = 
    let userPrefStore = new System.Collections.Generic.Dictionary<_,_>()
    let userColStore = new System.Collections.Generic.Dictionary<_, _>()
    
    interface System.IDisposable with
        member x.Dispose() =
            for (imap:ImapConnection) in userColStore.Values do
                imap.Processor.Disconnect () 
                |> Async.RunSynchronously
                //|> Option.map raise |> ignore

    interface Yaaf.Xmpp.MessageArchiving.IMessageArchivingStore with
        member x.GetPreferenceStore jid =
            match userPrefStore.TryGetValue jid.BareId with
            | true, v -> Task.FromResult v
            | false, _ -> 
                let newStore = Yaaf.Xmpp.MessageArchiving.MemoryUserPreferenceStore(jid.BareJid) :> Yaaf.Xmpp.MessageArchiving.IUserPreferenceStore
                userPrefStore.Add(jid.BareId, newStore)
                Task.FromResult newStore
        member x.GetArchiveStore jid  =
            let imap =
                match userColStore.TryGetValue jid.BareId with
                | true, v -> v
                | false, _ -> 
                    let wrapper = SimpleWrapper(ActiveUpImap4.createProcessor())
                    // login to your favorite imap server for testing...
                    let imap = ImapConnection(wrapper, "imap.yaaf.de", "ldapuser", "_______")
                    imap.DoWork
                        (fun worker -> 
                            async {
                                do! worker.SelectMailbox "chats-testing" |> Async.Ignore
                                do! worker.DeleteMailbox()
                                do! worker.SelectMailbox "chats-testing" |> Async.Ignore
                            }) 
                        |> Async.RunSynchronously
                    userColStore.Add(jid.BareId, imap)
                    imap
            let user = jid.BareJid
            ImapArchiveManager.convertImapToArchiveManager
                "chats-testing" 
                (GoogleChat.MessageMapper.createMessageMapper user)
                user
                imap
              :> Yaaf.Xmpp.MessageArchiving.IUserArchivingStore
              |> Task.FromResult 

[<TestFixture>]
type YaafImapArchiveManagerTest() = 
    //inherit Yaaf.Xmpp.IMTest.MessageArchivingStoreTest()
    //
    //override x.CreateArchivingStore () = 
    //    new YaafImapMessageArchivingStoreTest() :> Yaaf.Xmpp.IM.MessageArchiving.Core.MessageArchivingStore


    [<Test>]
    member this.``Dummy`` () = 
        ()