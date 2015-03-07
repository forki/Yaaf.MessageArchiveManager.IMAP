#light (*
    exec fsharpi --exec $0 --quiet
*)
// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
// NOTE: use build.cmd or build.sh for this to work!
//#silentCd "/home/reddragon/projects/xmpp/build/"
#I "../../../build"
//#silentCd @"C:\projects\xmpp\build"
#r "System.dll"
#r "System.Core.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "FSharpx.Core.dll"
#r "Yaaf.Logging.dll"
#r "Yaaf.RefactorOut.dll"
#r "Mono.System.Xml.dll"
#r "Yaaf.DependencyInjection.dll"
#r "Yaaf.Xmpp.Runtime.Core.dll"
#r "Yaaf.Sasl.dll"
#r "Yaaf.Xml.dll"
#r "ActiveUp.Net.Common.dll"
#r "ActiveUp.Net.Imap4.dll"
#r "Yaaf.DependencyInjection.Ninject.dll"
#r "Yaaf.Xmpp.Runtime.dll"
#r "Yaaf.Xmpp.IM.dll"
#r "Yaaf.Xmpp.IM.MessageArchiving.dll"
#r "Yaaf.Database.dll"
#r "Yaaf.Xmpp.IM.Sql.dll"
#r "Yaaf.Sasl.Ldap.dll"
#r "Yaaf.Xmpp.MessageArchiveManager.dll"
open System.Linq

open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager
#load "AbstractImap.fs"
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
#load "ImapArchiveManager.fs"
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
#load "ActiveUpImap4.fs"
#load "GoogleChat/ChatParser.fs"
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat
#load "GoogleChat/MessageMapper.fs"
#load "SyncedImap.fs"
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
open System.Net
open System.IO
open System.Diagnostics
open System.Xml
open System.Xml.Linq
open System.Net.Sockets
open Yaaf.Logging
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
open Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat
open ActiveUp.Net.Mail
open System.Threading
open Yaaf.Helper
open FSharp.Control
open System.Diagnostics
let logger = new System.Diagnostics.ConsoleTraceListener()
logger.Filter <- new EventTypeFilter(SourceLevels.All)
logger.TraceOutputOptions <- TraceOptions.DateTime
let setSource (source:TraceSource) =     
    source.Listeners.Clear()
    source.Switch.Level <- SourceLevels.Verbose
    source.Listeners.Add logger
setSource Log.UnhandledSource
logger.WriteLine "Test"


let imapImpl = ActiveUpImap4.createProcessor()
let wrapper = SimpleWrapper(imapImpl)
let con = ImapConnection(wrapper, "imap.yaaf.de", "ldap", "ldap#password")

//let command = "search charset utf-8 SENTON \"31-Jul-2014\""
//con.RawCommand "select Chats.Xmpp" |> Async.RunSynchronously
//con.RawCommand command |> Async.RunSynchronously
//let chatParser = ChatParser.createParser()
let user = JabberId.Parse "ldap@devel-xmpp.yaaf.de"
let messageMapper = MessageMapper.createMessageMapper user
let archiveManager = ImapArchiveManager.convertImapToArchiveManager "Chats.Xmpp" messageMapper user con

let jid = JabberId.Parse "kramer.tom\\40gmail.com@gtalk.devel-xmpp.yaaf.de"
let date = System.DateTime(2014,7,30,13,59,56)

archiveManager.QueryConversation jid date
    |> AsyncSeq.await
    |> Async.RunSynchronously
    |> Seq.toList

archiveManager.TryRequestConversation jid date
    |> Async.RunSynchronously

(*
//let res = syncedImap.RawCommand "LIST \"\" \"*\""
let folder = syncedImap.SelectMailbox "[Google Mail]/Chats"
//
//let folder = syncedImap.SelectMailbox "[Gmail]/Drafts"

let archiveManager = ImapArchiveManager.convertImapToArchiveManager mapper imapImpl
let syncedArchiveManager = SyncedArchiveManager(archiveManager)
let result = syncedArchiveManager.QueryMessages (Some <| Query.Contains "ts server hat kein pw oder?")
for r in result do
    printfn "Chat with %s on %s" r.Collection.With.ShortId (r.Collection.StartTime.ToShortDateString())
    for m in r.Collection.Messages do
        printfn "%s: %s" m.Author.ShortId m.Message
printfn "%A" (result)

let allChats = archiveManager.QueryMessages None |> Async.RunSynchronously

syncedImap.Disconnect()
*)











