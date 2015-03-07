// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP

open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.FSharp.Control

type SyncedImap(asyncImap : IMAPProcessor) = 
    
    let check r = 
        let result = Async.RunSynchronously r
        match result with
        | Some exn -> raise exn
        | None -> ()
    
    member this.Connect server = 
        let res = asyncImap.PostAndAsyncReply(fun reply -> IMAPConnection.Connect(server, reply))
        check res
    
    member this.Authenticate user pass = 
        let res = asyncImap.PostAndAsyncReply(fun reply -> IMAPConnection.Authenticate(user, pass, reply))
        check res
    
    member this.Disconnect() = 
        let res = asyncImap.PostAndAsyncReply(fun reply -> IMAPConnection.Disconnect(reply))
        check res