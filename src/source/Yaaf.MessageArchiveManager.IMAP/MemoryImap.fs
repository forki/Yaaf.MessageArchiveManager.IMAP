// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.FSharp.Control
open Yaaf.Logging

type ProcessorState = {
    CurrentUID : int
    AllMails : ((string * Email) list)
    CurrentPath : string option }
type MemoryImapProcessor() =
    static member GetImap () =
        let proc = MemoryImapProcessor.CreateProcessor()
        new SimpleWrapper(proc)
    static member CreateProcessor () =
        let fromState (client:unit) (state:ProcessorState) = 
            let currentState = ref state
            (fun () -> !currentState),
            { new IImapWorker with
                member x.DeleteEmail email =
                    if (email.UID.IsNone) then failwith "Email which has to be deleted has to be specified by UID!"
                    currentState :=
                        { !currentState with
                            AllMails = (!currentState).AllMails |> List.filter (fun (path, mail) -> mail.UID.Value <> email.UID.Value)
                        }
                    async.Return ()
                member x.DeleteMailbox () =
                    let mailboxPath = (!currentState).CurrentPath.Value
                    currentState :=
                        { !currentState with
                            AllMails = (!currentState).AllMails |> List.filter (fun (path, mail) -> path <> mailboxPath)
                        }
                    async.Return ()
                member x.Query query =
                    let mailboxPath = (!currentState).CurrentPath.Value
                    let mailboxMails = 
                        (!currentState).AllMails |> List.filter (fun (path, mail) -> path = mailboxPath) |> List.map snd
                        |> List.filter (query.FilterMail)
                    mailboxMails
                    |> AsyncSeq.ofSeq
                    |> async.Return
                member x.SelectMailbox path =
                    currentState := 
                        { !currentState with
                            CurrentPath = Some path
                        }
                    let folderInfo =
                        {
                            SubFolders = AsyncSeq.empty
                            Name = path
                            FullName = path
                            Messages = AsyncSeq.empty
                        }
                    async.Return folderInfo
                member x.SetEmail mail =
                    let path = (!currentState).CurrentPath.Value
                    let currentUid = (!currentState).CurrentUID
                    let mailUid, nextUid, filter = 
                        match mail.UID with
                        | Some uid -> uid, currentUid, (fun (p:string,e:Email) -> p <> path || e.UID.Value <> uid)
                        | None -> currentUid, currentUid + 1, (fun (p, e:Email) -> true)
                    let mail = { mail with UID = Some mailUid }
                    currentState := 
                        { !currentState with
                            AllMails = 
                                (path, mail) :: ((!currentState).AllMails |> List.filter filter)
                            CurrentUID = nextUid
                        }
                    async.Return () 
            }
        let processor = 
            new IMAPProcessor(fun inbox ->        
                let rec auth (client:unit) currentState = async {
                        let! msg = inbox.Receive()
                        match msg with 
                        | Authenticate (username, password, reply) ->
                            // Already authenticated!
                            reply.Reply <| None
                            return! auth client currentState
                        | Connect (host, reply) ->
                            // Already authenticated!                            
                            reply.Reply <| None
                            return! auth client currentState
                        | Noop (reply) ->
                            // Already authenticated!                            
                            reply.Reply <| None
                            return! auth client currentState
                        | Disconnect (reply) ->
                            try
                                printfn "start disconnect..."
                                reply.Reply None
                            with
                                exn ->
                                reply.Reply <| (Some <| (ImapException ("could not disconnect", exn) :> exn))
                            // Throw away the session anyway
                            return! disconnected()
                                
                        | DoWork (work, reply) ->
                            let getNewState, worker = fromState client currentState
                            try
                                let! result = work worker
                                reply.Reply (Choice1Of2 <| result)
                            with exn ->
                                reply.Reply (Choice2Of2 <| (ImapException("unknown exn", exn) :> exn))
                            return! auth client (getNewState())
#if INTERACTIVE
                        | RawCommand (cmd, reply) ->
                            try
                                let! result = command client cmd
                                reply.Reply (Choice1Of2 <| result)
                            with exn ->
                                reply.Reply (Choice2Of2 <| exn)
                            return! auth client mailbox
#endif
                    }
                and unauth (client:unit) = async {
                        // wait for authentication
                        let! maybeAuth, (reply:AsyncReplyChannel<exn option>) = 
                            inbox.Scan
                                (fun msg ->
                                    match msg with 
                                    | Authenticate(username, password, reply) ->
                                        Some (async { return (Some(username, password), reply) })
                                    | Disconnect(reply) ->
                                        Some (async { return (None, reply) })
                                    | _ -> None)
                        try
                            match maybeAuth with
                            | Some (username, password) ->
                                printfn "start auth..."
                                reply.Reply None
                                return! auth client { CurrentUID = 1; AllMails = []; CurrentPath = None }
                            | None ->
                                return! disconnected()
                        with exn ->
                            reply.Reply (Some <| exn)
                            return! unauth client
                    }
                and disconnected () = async {
                        // wait for connection
                        let! maybehost, (reply:AsyncReplyChannel<exn option>)  = 
                            inbox.Scan
                                (fun msg ->
                                    match msg with 
                                    | Connect(host, reply) -> Some (async { return Some host, reply })
                                    | Disconnect(reply) -> Some (async { return None, reply })
                                    | _ -> None )
                        try
                            match maybehost with
                            | Some host ->
                                printfn "start connect..."
                                reply.Reply None
                                return! unauth ()
                            | None ->
                                return! disconnected ()
                        with exn ->                            
                            reply.Reply (Some <| exn)
                            return! disconnected ()
                    }
                disconnected ())
        processor.Error
            |> Event.add (fun e -> 
                System.Console.WriteLine ("Mailbox crashed! {0}", e)
                System.Environment.Exit (-1)
                raise e
                )
        processor.Start()
        processor