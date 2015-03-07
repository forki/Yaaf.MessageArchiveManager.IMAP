// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP

open Yaaf.Helper
open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.FSharp.Control
open Yaaf.Logging

type ParsedCollection = 
    | DeletedCollection of Yaaf.Xmpp.MessageArchiving.ChangeItemData
    | NormalCollection of MessageCollection
    
    member x.Id = 
        match x with
        | DeletedCollection changeItem -> changeItem.Id
        | NormalCollection col -> col.Header.Id
    
    member x.Change = 
        match x with
        | DeletedCollection changeItem -> 
            { Data = changeItem
              Type = Yaaf.Xmpp.MessageArchiving.ChangeItemType.Removed } : Yaaf.Xmpp.MessageArchiving.ChangeItem
        | NormalCollection col -> 
            { Data = 
                  { Id = col.Header.Id
                    Version = col.Header.Version.Value }
              Type = Yaaf.Xmpp.MessageArchiving.ChangeItemType.Changed }

/// A Messagemapper controlls how Emails are mapped to MessageCollections, not all emails are messagecollections!
type IMessageMapper = 
    abstract ConvertEmailToMessageCollection : Email -> ParsedCollection option
    abstract ConvertMessageCollectionToEmail : ParsedCollection -> Email

type IImapExtendedUserArchiveManager = 
    inherit IExtendedUserArchivingStore
#if INTERACTIVE
    /// Debugging purposes
    abstract QueryEmails : Query option -> AsyncSeq<Email>
    abstract QueryConversation : JabberId -> System.DateTime -> AsyncSeq<Email * ParsedCollection>
    abstract TryRequestConversation : JabberId -> System.DateTime -> Async<(Email * ParsedCollection) option>
#endif


module ImapArchiveManager = 
    // TODO: The MessageMapper should maybe take care of this 
    // (because it could implement encrytion for example; Contains queries are then quite more complicated) 
    let rec convertMessageQueryToImap (query : Query) = 
        match query with
        | Query.And(left, right) -> ImapQuery.And(convertMessageQueryToImap left, convertMessageQueryToImap right)
        | Query.Or(left, right) -> ImapQuery.Or(convertMessageQueryToImap left, convertMessageQueryToImap right)
        | Query.Not(op) -> ImapQuery.Not(convertMessageQueryToImap op)
        | Query.OnDate date -> ImapQuery.SentOn date
        | Query.Contains content -> ImapQuery.Body content
        | Query.With jid -> ImapQuery.Or(ImapQuery.From jid.BareId, ImapQuery.To jid.BareId)
        | Query.Between(date, span) -> ImapQuery.And(ImapQuery.SentSince date, ImapQuery.SentBefore(date + span))
        | Query.ChangedSince(date) -> ImapQuery.Since date
    
    let convertImapToArchiveManager (mailbox : string) (mapping : IMessageMapper) (user : JabberId) (processor : ImapConnection) = 
        //let logger = Log.childTracer source "ImapArchiveManager"
        let stripDate (d:System.DateTime) =
            System.DateTime (d.Year, d.Month, d.Day, d.Hour, d.Minute, d.Second)
        let queryEmails (query : Query option) = 
            asyncSeq { 
                let imapQuery = 
                    match query with
                    | Some query -> convertMessageQueryToImap query
                    | None -> ImapQuery.All
                Log.Verb(fun _ -> L "posting query %A" imapQuery)
                let! results = processor.Query mailbox imapQuery // processor.PostAndAsyncReply(fun reply -> Query(imapQuery, reply))            
                Log.Verb(fun _ -> L "returning IMAP query results")
                yield! results
            }
        
        let queryMessages (query : Query option) = 
            queryEmails query
            |> AsyncSeq.map mapping.ConvertEmailToMessageCollection
            |> AsyncSeq.filter Option.isSome
            |> AsyncSeq.map Option.get
        
        let queryMessagesNotDeleted (query : Query option) = 
            queryMessages query |> AsyncSeq.choose (fun parsed -> 
                                       match parsed with
                                       | DeletedCollection _ -> None
                                       | NormalCollection col -> Some col)
        let queryConversation jid date =
            queryEmails (Some(Query.And(Query.OnDate date, Query.With jid)))
            |> AsyncSeq.map (fun email -> email, mapping.ConvertEmailToMessageCollection email)
            |> AsyncSeq.filter (fun (email, m) -> m.IsSome)
            |> AsyncSeq.map (fun (email, m) -> email, m.Value)
            |> AsyncSeq.filter (fun (email, m) -> m.Id.With.BareId = jid.BareId)
            
        let tryRequestEmailFor jid date = 
            async { 
                let! fetched = queryConversation jid date |> AsyncSeq.await
                // Now we can sort
                let sorted = 
                    fetched |> Seq.sortBy (fun (email, msg) -> 
                                   let diff = msg.Id.Start - date
                                   let ticks = abs diff.Ticks
                                   Log.Verb(fun _ -> L "DIFF (for sort) IS: %A (ticks: %d)" diff ticks)
                                   ticks)
                
                let bestGuess = sorted |> Seq.tryFind (fun _ -> true)
                return match bestGuess with
                       | None -> None
                       | Some(email, msg) -> 
                           let diff = msg.Id.Start - date
                           let secs = abs diff.TotalSeconds
                           Log.Verb(fun _ -> L "DIFF (FOR guess) IS: %A (secs: %f)" diff secs)
                           if secs > 0.9 then None
                           else bestGuess
            }
            |> Log.TraceMe
        
        let tryRequestEmailForNotDeleted jid date = 
            async { 
                let! maybe = tryRequestEmailFor jid date
                return match maybe with
                       | Some(email, m) -> 
                           match m with
                           | DeletedCollection _ -> None
                           //raise <| System.Collections.Generic.KeyNotFoundException("given collection was deleted")
                           | NormalCollection col -> Some(email, col)
                       | None -> None //raise <| System.Collections.Generic.KeyNotFoundException("given collection was not found")
            }
            |> Log.TraceMe
        
        let requestEmailFor jid date = 
            async { 
                let! maybe = tryRequestEmailFor jid date
                return match maybe with
                       | Some(email, m) -> email, m
                       | None -> raise <| System.Collections.Generic.KeyNotFoundException("given collection was not found")
            }
            |> Log.TraceMe
        
        let requestEmailForNotDeleted jid date = 
            async { 
                let! maybe = tryRequestEmailForNotDeleted jid date
                return match maybe with
                       | Some(email, m) -> email, m
                       | None -> raise <| System.Collections.Generic.KeyNotFoundException("given collection was not found")
            }
            |> Log.TraceMe
        
        let requestCollection jid date = async { let! email, m = requestEmailForNotDeleted jid date
                                                 return m } |> Log.TraceMe
        
        let storeCollection (col : MessageCollection) = 
            async { 
                let! maybe = tryRequestEmailForNotDeleted col.Header.Id.With col.Header.Id.Start
                let uid, newCol = 
                    match maybe with
                    | Some(email, currentCol) -> 
                        // update
                        Log.Info (fun () -> L "Updating exisiting email.")
                        email.UID, 
                        { currentCol with 
                            Header = 
                                { currentCol.Header with 
                                    Version = Some(currentCol.Header.Version.Value + 1)
                                    Thread =
                                        match col.Header.Thread with
                                        | Some s -> Some s
                                        | None -> currentCol.Header.Thread 
                                    Subject =
                                        match col.Header.Subject with
                                        | Some s -> Some s
                                        | None -> currentCol.Header.Subject}
                            // append chat items
                            ChatItems = currentCol.ChatItems @ col.ChatItems
                            // set next or previous
                            SetPrevious = col.SetPrevious
                            Previous = 
                                if col.SetPrevious then
                                    col.Previous
                                else currentCol.Previous
                            SetNext = col.SetNext
                            Next = 
                                if col.SetNext then
                                    col.Next
                                else currentCol.Next }
                    | None -> 
                        Log.Info (fun () -> L "Creating new email.")
                        None, { col with Header = { col.Header with Version = None } }
                let! res = processor.SetEmail mailbox { (mapping.ConvertMessageCollectionToEmail(NormalCollection newCol)) with UID = uid }
                return ()
            }
            |> Log.TraceMe
        
        let removeCollection jid date = 
            async { 
                let! maybe = tryRequestEmailFor jid date
                match maybe with
                | Some(email, m) -> 
                    match m with
                    | NormalCollection col -> 
                        let deleteData = { mapping.ConvertMessageCollectionToEmail(DeletedCollection m.Change.Data) with UID = email.UID }
                        do! processor.SetEmail mailbox deleteData
                        return true
                    | DeletedCollection _ -> return false
                | None -> return false
            }
            |> Log.TraceMe
        
        let queryArchive query = 
            queryMessagesNotDeleted query
            |> AsyncSeq.await
            |> Async.RunSynchronously
            |> List.ofSeq
        
        let buildQueryFromFilter (filter : Yaaf.Xmpp.MessageArchiving.CollectionFilter) = 
            if filter.With.IsSome then Some <| Query.With filter.With.Value
            else None
        
        let removeCollections query = 
            async { 
                for email in queryEmails query do
                    do! processor.DeleteEmail mailbox email
            }
            |> Log.TraceMe
        
        let getChangesSince date = 
            async { 
                let msgs = queryEmails (Some(Query.ChangedSince date))
                // Fetch all queried items
                // Fetch all messages
                let! fetched = msgs
                               |> AsyncSeq.map (fun email -> mapping.ConvertEmailToMessageCollection email)
                               |> AsyncSeq.choose (fun m -> m)
                               |> AsyncSeq.map (fun m -> m.Change)
                               |> AsyncSeq.await
                // sort?
                return fetched |> Seq.toList
            }
            |> Log.TraceMe
        
        { new IImapExtendedUserArchiveManager with
              
              /// Append messages to the given collection.
              //abstract member AppendMessages : MessageCollection -> SimpleMessage seq -> Async<unit>    
              member x.StoreCollection col = 
                let stripCol = { col with Header = { col.Header with Id = {col.Header.Id with Start = stripDate(col.Header.Id.Start)}}}
                storeCollection stripCol |> Async.StartAsTaskImmediate :> System.Threading.Tasks.Task
              
              /// Queries the message archive for all messages with the given string (use * for all).
              /// The first parameter specifies the given time and the secound parameter the user.
              //abstract member QueryMessages : (DateTime * TimeSpan) option -> JabberId option -> string -> AsyncMessages
              member x.FilterMessages filter = 
                queryMessagesNotDeleted (buildQueryFromFilter filter)
                |> AsyncSeq.await
                |> Async.map (Seq.map (fun c -> c.Header))
                |> Async.map Seq.toList
                |> Async.StartAsTaskImmediate
                //queryArchive (buildQueryFromFilter filter) |> List.map 
              
              /// Request a collection with the given person on the given date.
              //abstract member RequestCollection : JabberId -> DateTime -> Async<MessageCollectionContext>
              member x.RetrieveCollection header = requestCollection header.With (stripDate header.Start) |> Async.StartAsTaskImmediate
              
              /// Deletes the MessageCollections within the specified range (or only one if the secound parameter is None)
              /// Deletes ALL MessageCollections when both parameter are None.
              //abstract member DeleteMessageCollections :  DateTime option -> TimeSpan option -> Async<unit>       
              member x.RemoveCollection header = removeCollection header.With (stripDate header.Start) |> Async.StartAsTaskImmediate
              
              /// Get all message archives which where changed since the given date (for archive replication)
              member x.GetChangesSince date = getChangesSince (stripDate date) |> Async.StartAsTaskImmediate
              
              // TODO: Use the metadata saved along with the emails to figure this out
              //failwith "not implemented"
              /// Queries the message archive for all messages with the given string (use * for all).
              /// The first parameter specifies the given time and the secound parameter the user.
              //abstract member QueryMessages : (DateTime * TimeSpan) option -> JabberId option -> string -> AsyncMessages
              member x.QueryMessages query = queryMessagesNotDeleted query
              
              /// Deletes the MessageCollections within the specified range (or only one if the secound parameter is None)
              /// Deletes ALL MessageCollections when both parameter are None.
              //abstract member DeleteMessageCollections :  DateTime option -> TimeSpan option -> Async<unit>       
              member x.RemoveCollections query = removeCollections query
#if INTERACTIVE
              /// Debugging purposes
              member x.QueryEmails query = queryEmails query
              member x.QueryConversation jid date = queryConversation jid (stripDate date)
              member x.TryRequestConversation jid date = tryRequestEmailFor jid (stripDate date)
#endif
        }
