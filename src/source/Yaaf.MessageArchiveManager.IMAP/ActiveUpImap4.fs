// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP

open System
open System.Net
open System.Net.Sockets
open System.Xml.Linq
open ActiveUp.Net.Mail
open Yaaf.FSharp.Control
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
open Yaaf.Helper
open Yaaf.Logging

type Message = ActiveUp.Net.Mail.Message

module ActiveUpImap4 = 
    let decString (s : System.String) = 
        if (Environment.NewLine <> "\r\n") then s.Replace("\r\n", "\n").Replace("\n", Environment.NewLine)
        else s
    
    let parseMessage (message : Message) id = 
        { SequenceNumber = Some id
          Date = message.Date
          ReceivedDate = message.ReceivedDate
          Subject = message.Subject
          UID = Some message.Id
          MessageID = message.MessageId
          To = 
              message.To |> Seq.map (fun t -> 
                                { Name = t.Name
                                  Address = t.Email })
          From = 
              [ { Name = message.From.Name
                  Address = message.From.Email } ]
          XmlContent = 
              message.LeafMimeParts
              |> Seq.cast
              |> Seq.filter (fun (t : MimePart) -> t.ContentType.Type = "text")
              |> Seq.filter (fun (t : MimePart) -> t.ContentType.SubType = "xml")
              |> Seq.map (fun t -> decString t.TextContent)
              |> Seq.map XElement.Parse
              |> Seq.tryFind (fun _ -> true)
          HtmlContent = decString message.BodyHtml.Text
          TextContent = decString message.BodyText.Text }
    
    let encString (s : System.String) = 
        if (Environment.NewLine <> "\r\n") then s.Replace("\r\n", "\n").Replace("\n", "\r\n")
        else s
    
    let toMessage (email : Email) = 
        let m = Message()
        m.Date <- email.Date
        m.Trace.Add(new TraceInfo("yaaf.de", email.ReceivedDate, "archivemanager.xmpp.yaaf.de"))
        m.Subject <- email.Subject
        if email.UID.IsSome then m.Id <- email.UID.Value
        //m.MessageId <- email.MessageID
        let toCol = new AddressCollection()
        toCol.AddRange(email.To |> Seq.map (fun t -> new Address(t.Address, t.Name)))
        m.To <- toCol
        let first = email.From |> Seq.head
        m.From <- new Address(first.Address, first.Name)
        if email.XmlContent.IsSome then 
            let part = new MimePart()
            part.Charset <- "utf-8"
            part.ParentMessage <- m
            part.ContentType.Type <- "text"
            part.ContentType.SubType <- "xml"
            // part.ContentDisposition <- ContentDisposition.
            part.ContentTransferEncoding <- ContentTransferEncoding.SevenBits
            //part.ContentT
            part.TextContent <- email.XmlContent.Value.ToString()
            m.EmbeddedObjects.Add(part)
        m.BodyHtml.Text <- email.HtmlContent
        m.BodyText.Text <- email.TextContent
        m
    
    let mailToMimeString (mail : Message) = mail.ToMimeString() |> encString
    
    type Client = Imap4Client
    
    module Imap4ClientModule = 
        /// returns The server's response greeting.
        let connectSSL (client : Client) host port = 
            let sslHandShake = new ActiveUp.Net.Security.SslHandShake(host)
            sslHandShake.ServerCertificateValidationCallback <- new System.Net.Security.RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
            Async.FromBeginEnd((fun (callback, obj) -> client.BeginConnectSsl(host, port, sslHandShake, callback)), client.EndConnectSsl)
        
        /// returns The server's response greeting.
        let authenticate (client : Client) mech username password = 
            Async.FromBeginEnd((fun (callback, obj) -> client.BeginAuthenticate(username, password, mech, callback)), client.EndAuthenticate)
        
        /// <summary>
        /// Sends the command to the server.
        /// The command tag is automatically added.
        /// </summary>
        /// <param name="command">The command (with arguments if necesary) to be sent.</param>
        /// <returns>The server's response.</returns>
        let command (client : Client) command = 
            let cmd = Async.FromBeginEnd((fun (callback, obj) -> client.BeginCommand(command, callback)), client.EndCommand)
            async { 
                Log.Verb(fun _ -> L "Starting command %s" command)
                let! cmdResult = cmd
                Log.Verb(fun _ -> L "Result: \"%s\"\n\nUnescaped: %s" (cmdResult.Replace("\n", "\\n").Replace("\r", "\\r")) cmdResult)
                return cmdResult
            }
        
        let loginFast (client : Client) username password = command client ("login " + username + " " + password)
        
        /// <summary>
        /// Creates a mailbox with the specified name.
        /// </summary>
        /// <param name="mailboxName">The name of the new mailbox.</param>
        /// <returns>The newly created mailbox.</returns>
        let createMailbox (client : Client) mailbox = Async.FromBeginEnd((fun (callback, obj) -> client.BeginCreateMailbox(mailbox, callback)), client.EndCreateMailbox)
        
        /// <summary>
        /// Deletes a mailbox.
        /// </summary>
        /// <param name="mailboxName">The mailbox to be deleted.</param>
        /// <returns>The server's response.</returns>
        let deleteMailbox (client : Client) mailbox = Async.FromBeginEnd((fun (callback, obj) -> client.BeginDeleteMailbox(mailbox, callback)), client.EndDeleteMailbox)
        
        /// <summary>
        /// Same as SelectMailbox() except that the mailbox is opened with read-only permission.
        /// </summary>
        /// <param name="mailboxName">The mailbox to be examined.</param>
        /// <returns>The examined mailbox.</returns>
        let examineMailbox (client : Client) mailbox = Async.FromBeginEnd((fun (callback, obj) -> client.BeginExamineMailbox(mailbox, callback)), client.EndExamineMailbox)
        
        /// Removes all messages marked with the Deleted flag.
        let expunge (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginExpunge(callback)), client.EndExpunge)
        
        /// <summary>
        /// Retrieves a list of mailboxes.
        /// </summary>
        /// <param name="reference">The base path.</param>
        /// <param name="mailboxName">Mailbox name.</param>
        /// <returns>A MailboxCollection object containing the requested mailboxes.</returns>
        let getMailboxes (client : Client) reference mailbox = 
            Async.FromBeginEnd((fun (callback, obj) -> client.BeginGetMailboxes(reference, mailbox, callback)), client.EndGetMailboxes)
        
        /// <summary>
        /// Fills in or refreshes the Imap4Client.AllMailboxes and Imap4Client.Mailboxes properties.
        /// </summary>
        let loadMailboxes (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginLoadMailboxes(callback)), client.EndLoadMailboxes)
        
        /// <summary>
        /// Performs a NOOP command which is used to maintain the connection alive.
        /// </summary>
        /// <returns>The server response.</returns>
        /// <remarks>Some servers include mailbox update informations in the response.</remarks>        
        let noop (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginNoop(callback)), client.EndNoop)
        
        /// Equivalent to Noop().
        let check (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginCheck(callback)), client.EndCheck)
        
        /// <summary>
        /// Renames a mailbox.
        /// </summary>
        /// <param name="oldMailboxName">The mailbox to be renamed.</param>
        /// <param name="newMailboxName">The new name of the mailbox.</param>
        /// <returns>The server's response.</returns>
        let renameMailbox (client : Client) oldName newName = 
            Async.FromBeginEnd((fun (callback, obj) -> client.BeginRenameMailbox(oldName, newName, callback)), client.EndRenameMailbox)
        
        /// <summary>
        /// Selects a mailbox on the server.
        /// </summary>
        /// <param name="mailboxName">The mailbox to be selected.</param>
        /// <returns>The selected mailbox.</returns>
        let selectMailbox (client : Client) mailbox = Async.FromBeginEnd((fun (callback, obj) -> client.BeginSelectMailbox(mailbox, callback)), client.EndSelectMailbox)
        
        /// <summary>
        /// Subscribes to a mailbox.
        /// </summary>
        /// <param name="mailboxName">The mailbox to be subscribed to.</param>
        /// <returns>The server's response.</returns>
        let subscribeMailbox (client : Client) mailbox = Async.FromBeginEnd((fun (callback, obj) -> client.BeginSubscribeMailbox(mailbox, callback)), client.EndSubscribeMailbox)
        
        /// <summary>
        /// Unsubscribes from a mailbox.
        /// </summary>
        /// <param name="mailboxName">The mailbox to be unsubscribed from.</param>
        /// <returns>The server's response.</returns>
        let unsubscribeMailbox (client : Client) mailbox = 
            Async.FromBeginEnd((fun (callback, obj) -> client.BeginUnsubscribeMailbox(mailbox, callback)), client.EndUnsubscribeMailbox)
        
        /// <summary>
        /// Closes the mailbox and removes messages marked with the Deleted flag.
        /// </summary>
        /// <returns>The server's response.</returns>        
        let closeMailbox (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginClose(callback)), client.EndClose)
        
        /// returns The server's response greeting.
        let disconnect (client : Client) = Async.FromBeginEnd((fun (callback, obj) -> client.BeginDisconnect(callback)), client.EndDisconnect)
        
        let fetchMessageRaw (client : Client) (mailbox : Mailbox) (messageOrdinal : int) markMessagesAsRead (extraDataItemToFetch : string seq) = 
            async { 
                let! res = selectMailbox client mailbox.Name
                let dataItemsToRetrieve = 
                    System.String.Join(" ", 
                                       [ (if markMessagesAsRead then "rfc822"
                                          else "body.peek[]") ]
                                       |> Seq.append extraDataItemToFetch)
                return! command client ("fetch " + messageOrdinal.ToString() + " (" + dataItemsToRetrieve + ")")
            }
    
    open Imap4ClientModule
    
    // type Mailbox = Mailbox     
    module ImapMailbox = 
        /// <summary>
        /// Searches the mailbox for messages corresponding to the query.
        /// </summary>
        /// <param name="query">Query to use.</param>
        /// <returns>An array of integers containing ordinal positions of messages matching the query.</returns>
        let search (mailbox : Mailbox) charset query = 
            async { 
                let! response = command mailbox.SourceClient ("search charset " + charset + " " + query)
                let decodedResponse = decString response
                let lines = decodedResponse.Split([| Environment.NewLine |], System.StringSplitOptions.None)
                let searchResult = lines |> Seq.find (fun line -> line.ToUpperInvariant().Contains "SEARCH")
                let parts = searchResult.Split(' ')
                return parts
                       |> Seq.skip 2
                       |> Seq.map (fun part -> System.Convert.ToInt32(part))
                       |> Seq.cache
            }
        
        /// <summary>
        /// Deletes the mailbox.
        /// </summary>
        /// <returns>The server's response.</returns>
        let delete (mailbox : Mailbox) = Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginDelete(callback)), mailbox.EndDelete)
        
        let rename (mailbox : Mailbox) newName = Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginRename(newName, callback)), mailbox.EndRename)
        let moveMessage (mailbox : Mailbox) messageId newMailbox = 
            Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginMoveMessage(messageId, newMailbox, callback)), mailbox.EndMoveMessage)
        let copyMessage (mailbox : Mailbox) messageId newMailbox = 
            Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginCopyMessage(messageId, newMailbox, callback)), mailbox.EndCopyMessage)
        let deleteMessage (mailbox : Mailbox) messageId expunge = 
            Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginUidDeleteMessage(messageId, expunge, callback)), mailbox.EndUidDeleteMessage)
        let appendMessageRfc822 (mailbox : Mailbox) (message : string) = Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginAppend(message, callback)), mailbox.EndAppend)
        let appendMessage (mailbox : Mailbox) (message : Message) = appendMessageRfc822 mailbox (mailToMimeString message)
        // NOTE: this is broken in unix (as it sends Environment.NewLine and should send \r\n)
        //Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginAppend(message, callback)), mailbox.EndAppend)
        let emptyMailbox (mailbox : Mailbox) (expunge : bool) = Async.FromBeginEnd((fun (callback, obj) -> mailbox.BeginEmpty(expunge, callback)), mailbox.EndEmpty)
        let parseMessageRfc822 (fetch : Fetch) (messageId) = Async.FromBeginEnd((fun (callback, obj) -> fetch.BeginMessageObject(messageId, callback)), fetch.EndMessageObject)
        let fetchMessageRfc822 (fetch : Fetch) (messageId) = Async.FromBeginEnd((fun (callback, obj) -> fetch.BeginMessageString(messageId, callback)), fetch.EndMessageString)
        let fetchMessageUid (fetch : Fetch) (messageId) = Async.FromBeginEnd((fun (callback, obj) -> fetch.BeginUid(messageId, callback)), fetch.EndUid)
        
        let createFetchTask (proc : SimpleWrapper) (mailbox : Mailbox) i = 
            new System.Threading.Tasks.Task<Email>(fun _ -> 
                let task = 
                    async { 
                        Log.Verb(fun _ -> L "Fetching Email %d" i)
                        let! msg, uid = proc.DoWork(fun worder -> 
                                            async { 
                                                let fetch = mailbox.Fetch
                                                let messageId = i
                                                let! message = fetchMessageRfc822 fetch messageId
                                                let! uid = fetchMessageUid fetch messageId
                                                return message, uid
                                            })
                        // At least distribute the parsing part
                        Log.Verb(fun _ -> L "Parsing Email %d" i)
                        let preParsed = ActiveUp.Net.Mail.Parser.ParseMessage(msg : string)
                        preParsed.Id <- uid
                        return parseMessage preParsed i
                    }
            
                let t = Async.StartAsTaskImmediate task
                t.Result)
        
        /// <summary>
        /// Search for messages accoridng to the given query.
        /// </summary>
        /// <param name="query">Query to use.</param>
        /// <returns>A collection of messages matching the query.</returns>
        let searchParse (proc : SimpleWrapper) (mailbox : Mailbox) charset query = 
            async { 
                Log.Verb(fun _ -> L "Starting query %s" query)
                let! data = search mailbox charset query
                Log.Verb(fun _ -> 
                    L "Finished query: %s" (String.Join(",", 
                                                        data
                                                        |> Seq.map (fun i -> i.ToString())
                                                        |> Seq.toArray)))
                return data
                       |> Seq.map (fun index -> createFetchTask proc mailbox index)
                       // To prevent creating the same tasks again
                       |> Seq.cache
            }
    
    open ImapMailbox
    
    let convertMailbox (mailbox : Mailbox) folder = 
        Log.Verb(fun _ -> L "Converting Mailbox %s with %d Messages" mailbox.Name mailbox.MessageCount)
        { Name = folder
          FullName = folder
          SubFolders = Seq.empty |> AsyncSeq.ofSeq
          Messages = AsyncSeq.empty }
    
    (*    Seq.init 
                    (mailbox.MessageCount + 1) 
                    (fun i -> createFetchTask mailbox i)
                // Skip item 0 as imap starts index at 1
                |> Seq.skip 1
                |> AsyncSeq.ofSeq
                |> Task.startTasks
                // Cache the sequence
                |> AsyncSeq.cache*)
    open QueryParser
    
    type ProcessorState = 
        { CurrentMailbox : Mailbox option
          CurrentPath : string option }
    
    let createProcessor() = 
        let fromState (proc : SimpleWrapper) (client : Client) (state : ProcessorState) = 
            let currentState = ref state
            (fun () -> !currentState), 
            { new IImapWorker with
                  member x.DeleteEmail email = deleteMessage (!currentState).CurrentMailbox.Value email.UID.Value false
                  
                  member x.DeleteMailbox() = 
                      async { 
                          // unselect mailbox
                          let mailboxToDelete = (!currentState).CurrentPath.Value
                          try 
                              let! closeRet = closeMailbox client
                              ()
                          with :? Imap4Exception -> ()
                          currentState := { CurrentMailbox = None
                                            CurrentPath = None }
                          let! delRet = deleteMailbox client mailboxToDelete
                          return ()
                      }
                  
                  member x.Query query = 
                      async { 
                          let queryString = parseQuery query
                          let! messageCollections = searchParse proc (!currentState).CurrentMailbox.Value "utf-8" queryString
                          return messageCollections
                                 |> AsyncSeq.ofSeq
                                 |> Task.startTasks
                                 |> AsyncSeq.choose (fun task -> 
                                        if task.IsFaulted then 
                                            Log.Err(fun _ -> L "Task faulted: %O" task.Exception)
                                            None
                                        else Some task.Result)
                                 |> AsyncSeq.cache
                      }
                  
                  member x.SelectMailbox path = 
                      async { 
                          match (!currentState).CurrentPath with
                          | Some p when p = path -> return convertMailbox (!currentState).CurrentMailbox.Value p
                          | _ -> 
                              let ret mailbox = 
                                  currentState := { CurrentMailbox = Some mailbox
                                                    CurrentPath = Some path }
                                  convertMailbox mailbox path
                              try 
                                  let! mailbox = selectMailbox client path
                                  return ret mailbox
                              with :? Imap4Exception -> let! mailbox = createMailbox client path
                                                        return ret mailbox
                      }
#if INTERACTIVE
                  member x.RawCommand cmd =
                    async { 
                        let! result = command client cmd
                        return result
                    }
#endif
                    
                  member x.SetEmail mail = 
                      async { 
                          // first append then delete the old version
                          let! ret = appendMessage (!currentState).CurrentMailbox.Value (toMessage { mail with UID = None })
                          match mail.UID with
                          | Some uid -> do! deleteMessage (!currentState).CurrentMailbox.Value uid true
                          | None -> ()
                          return ()
                      } }
        
        let processor = 
            new IMAPProcessor(fun inbox -> 
            let rec auth (client : Client) currentState = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | Authenticate(username, password, reply) -> 
                        // Already authenticated!
                        reply.Reply <| None // (Some <| ImapException(ImapExceptionType.AuthenticationFailure ("already authenticated!", None)))
                        return! auth client currentState
                    | Connect(host, reply) -> 
                        // Already authenticated!                            
                        reply.Reply <| None //<| (Some <| ImapException(ImapExceptionType.ConnectionFailure ("already connected!", None)))
                        return! auth client currentState
                    | Noop(reply) ->
                        let asy =
                            async {
                                try 
                                    let! res = noop client
                                    reply.Reply None
                                    return auth client currentState
                                with exn -> 
                                    reply.Reply(Some <| (ImapException("could not execute work item.", exn) :> exn))
                                    return disconnected()
                            }
                        let! a = asy
                        return! a
                    | Disconnect(reply) -> 
                        try 
                            Log.Verb(fun _ -> L "start disconnect...")
                            let! result = disconnect client
                            Log.Verb(fun _ -> L "received: %s" result)
                            reply.Reply None
                        with exn -> reply.Reply <| (Some <| (ImapException("could not disconnect", exn) :> exn))
                        // Throw away the session anyway
                        return! disconnected()
                    | DoWork(work, reply) -> 
                        let getNewState, worker = fromState (SimpleWrapper(inbox)) client currentState
                        let asy =
                            async {
                                try 
                                    let! result = work worker
                                    reply.Reply(Choice1Of2 <| result)
                                    return auth client (getNewState())
                                with exn -> 
                                    reply.Reply(Choice2Of2 <| (ImapException("could not execute work item.", exn) :> exn))
                                    return disconnected()
                            }
                        let! a = asy
                        return! a
                }
            
            and unauth (client : Client) = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | Authenticate(username, password, reply) -> 
                        try
                            Log.Verb(fun _ -> L "start auth...")
                            let! answer = loginFast client username password
                            reply.Reply None
                            Log.Verb(fun _ -> L "received: %s" answer)
                            return! auth client { CurrentMailbox = None
                                                  CurrentPath = None }
                        with exn -> 
                            reply.Reply(Some <| (ImapException("could not authenticate: ", exn) :> exn))
                            return! unauth client
                    | Connect(host, reply) -> 
                        // Already authenticated!                            
                        reply.Reply <| None //<| (Some <| ImapException(ImapExceptionType.ConnectionFailure ("already connected!", None)))
                        return! unauth client
                    | Noop(reply) ->
                        let asy =
                            async {
                                try 
                                    let! res = noop client
                                    reply.Reply None
                                    return unauth client
                                with exn -> 
                                    reply.Reply(Some <| (ImapException("could not execute work item.", exn) :> exn))
                                    return disconnected()
                            }
                        let! a = asy
                        return! a
                    | Disconnect(reply) -> 
                        try 
                            Log.Verb(fun _ -> L "start disconnect...")
                            let! result = disconnect client
                            Log.Verb(fun _ -> L "received: %s" result)
                            reply.Reply None
                        with exn -> reply.Reply <| (Some <| (ImapException("could not disconnect", exn) :> exn))
                        return! disconnected()
                    | DoWork(work, reply) -> 
                        reply.Reply <| (Choice2Of2 <| (ImapException("cannot do work in unauth state") :> exn))
                        return! unauth client
                }
            
            and disconnected() = 
                async { 
                
                    let! msg = inbox.Receive()
                    match msg with
                    | Authenticate(username, password, reply) -> 
                        reply.Reply <| (Some <| (ImapException("cannot do auth in disconnected state") :> exn))
                        return! disconnected()
                    | Connect(host, reply) -> 
                        try 
                            let client = new Imap4Client()
                            Log.Verb(fun _ -> L "start connect...")
                            let! greeting = connectSSL client host 993
                            reply.Reply None
                            Log.Verb(fun _ -> L "received: %s" greeting)
                            return! unauth client
                        with exn -> 
                            reply.Reply(Some <| (ImapException("could not connect..", exn) :> exn))
                            return! disconnected()
                    | Noop(reply) ->
                        reply.Reply <| (Some <| (ImapException("cannot do noop in disconnected state") :> exn))
                        return! disconnected()
                    | Disconnect(reply) -> 
                        reply.Reply None
                        return! disconnected()
                    | DoWork(work, reply) -> 
                        reply.Reply <| (Choice2Of2 <| (ImapException("cannot do work in disconnected state") :> exn))
                        return! disconnected()

                }
            
            disconnected())
        
        processor.Error |> Event.add (fun e -> 
                               // When the mailbox crashes the next calls will deadlock anyway
                               // So fail as hard as possible
                               let error = sprintf "IMAP Mailbox crashed! %O" e
                               Log.Crit(fun _ -> error)
                               Console.Error.WriteLine(error)
                               //Environment.Exit(-1)
                               //raise e
                               )
        processor.Start()
        processor
