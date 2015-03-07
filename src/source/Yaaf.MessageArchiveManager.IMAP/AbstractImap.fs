// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP

open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.FSharp.Control
open System
open System.Xml.Linq
open Yaaf.Logging

// Abstract away the concrete IMAP implementation.
// This way we can switch to any other implemenation if required.
/// Encapsulated a mail address
type MailAddress = 
    { /// The name of the user "Name OtherName" for example
      Name : string
      /// The address of the user "test@test.com" for example
      Address : String }

/// A message in an IMAP folder, abstracted away everything we don't need for message archiving
type Email = 
    { /// The sequence number of the mail within the mailbox
      SequenceNumber : int option
      /// The date of the Message (Sent date = key of the collection)
      Date : DateTime
      /// The date of the last change (Received date = date of last change)
      ReceivedDate : DateTime
      /// The Subject of the Message
      Subject : string
      /// The UID of the Message within the IMAP folder
      UID : int option
      /// Some Message ID (used as Thread in collections)
      MessageID : string
      /// The Receiver of the Message
      To : MailAddress seq
      /// The Sender of the Message
      From : MailAddress seq
      /// The XML Content of the Message
      XmlContent : XElement option // This is the Element we need
      /// The HTML Content of the Message
      HtmlContent : string // This controls how the chat is displayed in a regular client like thunderbird
      /// The Text Content of the Message
      TextContent : string }

/// Cached data for a given folder 
type FolderInfo = 
    { SubFolders : AsyncSeq<FolderInfo>
      Name : string
      FullName : string
      Messages : AsyncSeq<Email> }

type SequenceSet = int * int

type Flag = string

/// The IMAP-Query Syntax from rfc3501
type ImapQuery = 
    /// Messages that match both search keys. (in rcf no extra command)
    | And of ImapQuery * ImapQuery
    /// Messages with message sequence numbers corresponding to the
    /// specified message sequence number set.
    | SequenceSet of SequenceSet
    /// All messages in the mailbox; the default initial key for
    /// ANDing.
    | All
    /// Messages with the \Answered flag set.
    | Answered
    /// Messages that contain the specified string in the envelope
    /// structure's BCC field.
    | Bcc of string
    /// Messages whose internal date (disregarding time and timezone)
    /// is earlier than the specified date.
    | Before of DateTime
    /// Messages that contain the specified string in the body of the
    /// message.
    | Body of string
    /// Messages that contain the specified string in the envelope
    /// structure's CC field.
    | Cc of string
    /// Messages with the \Deleted flag set.
    | Deleted
    /// Messages with the \Draft flag set.
    | Draft
    /// Messages with the \Flagged flag set.
    | Flagged
    /// Messages that contain the specified string in the envelope
    /// structure's FROM field.
    | From of string
    /// Messages that have a header with the specified field-name (as
    /// defined in [RFC-2822]) and that contains the specified string
    /// in the text of the header (what comes after the colon).  If the
    /// string to search is zero-length, this matches all messages that
    /// have a header line with the specified field-name regardless of
    /// the contents.
    | Header of string * string
    /// Messages with the specified keyword flag set.
    | Keyword of Flag
    /// Messages with an [RFC-2822] size larger than the specified
    /// number of octets.
    | Larger of int
    /// Messages that have the \Recent flag set but not the \Seen flag.
    /// This is functionally equivalent to "(RECENT UNSEEN)".
    | New
    /// Messages that do not match the specified search key.
    | Not of ImapQuery
    /// Messages that do not have the \Recent flag set.  This is
    /// functionally equivalent to "NOT RECENT" (as opposed to "NOT
    /// NEW").
    | Old
    /// Messages whose internal date (disregarding time and timezone)
    /// is within the specified date. 
    | On of DateTime
    /// Messages that match either search key.
    | Or of ImapQuery * ImapQuery
    /// Messages that have the \Recent flag set.
    | Recent
    /// Messages that have the \Seen flag set.
    | Seen
    /// Messages whose [RFC-2822] Date: header (disregarding time and
    /// timezone) is earlier than the specified date.
    | SentBefore of DateTime
    /// Messages whose [RFC-2822] Date: header (disregarding time and
    /// timezone) is within the specified date.
    | SentOn of DateTime
    /// Messages whose [RFC-2822] Date: header (disregarding time and
    /// timezone) is within or later than the specified date. 
    | SentSince of DateTime
    /// Messages whose internal date (disregarding time and timezone)
    /// is within or later than the specified date.
    | Since of DateTime
    /// Messages with an [RFC-2822] size smaller than the specified
    /// number of octets.
    | Smaller of int
    /// Messages that contain the specified string in the envelope
    /// structure's SUBJECT field.
    | Subject of string
    /// Messages that contain the specified string in the header or
    /// body of the message.
    | Text of string
    /// Messages that contain the specified string in the envelope
    /// structure's TO field.
    | To of string
    /// Messages with unique identifiers corresponding to the specified
    /// unique identifier set.  Sequence set ranges are permitted.
    | Uid of SequenceSet
    /// Messages that do not have the \Answered flag set.
    | Unanswered
    /// Messages that do not have the \Deleted flag set.
    | Undeleted
    /// Messages that do not have the \Draft flag set.
    | Undraft
    /// Messages that do not have the \Flagged flag set.
    | Unflagged
    /// Messages that do not have the specified keyword flag set.
    | Unkeyword of Flag
    /// Messages that do not have the \Seen flag set.| 
    | Unseen
    // TODO: This method is incomplete
    member query.FilterMail(email : Email) = 
        match query with
        | SequenceSet(set) -> 
            let item1, item2 = set
            item1 < email.SequenceNumber.Value && email.SequenceNumber.Value < item2
        | And(left, right) -> left.FilterMail email && right.FilterMail email
        | All -> true
        | Answered -> true
        | Bcc(bcc) -> true
        | Before(date) -> email.Date <= date
        | Body(body) -> true
        | Cc(cc) -> true
        | Deleted -> true
        | Draft -> true
        | Flagged -> true
        | From(from) -> true
        | Header(header, content) -> true
        | Keyword(flag) -> true
        | Larger(n) -> true
        | New -> true
        | Not(query) -> not (query.FilterMail email)
        | Old -> true
        | On(date) -> email.ReceivedDate.Date = date.Date
        | Or(queryLeft, queryRight) -> queryLeft.FilterMail email || queryRight.FilterMail email
        | Recent -> true
        | Seen -> true
        | SentBefore(date) -> true
        | SentOn(date) -> email.Date.Date = date.Date
        | SentSince(date) -> date <= email.Date
        | Since(date) -> date <= email.ReceivedDate
        | Smaller(n) -> true
        | Subject(subject) -> true
        | Text(text) -> true
        | To(toString) -> true
        | Uid(set) -> 
            let item1, item2 = set
            item1 < email.UID.Value && email.UID.Value < item2
        | Unanswered -> true
        | Undeleted -> true
        | Undraft -> true
        | Unflagged -> true
        | Unkeyword(flag) -> true
        | Unseen -> true

module QueryParser = 
    let parseSequenceSet set = 
        let item1, item2 = set
        if (item1 > item2) then failwith "invalid sequence"
        sprintf "%d:%d" item1 item2
    
    let parseDate (date : DateTime) = 
        // "01-Jan-2010"
        date.ToString("\\\"dd-MMM-yyyy\\\"", System.Globalization.CultureInfo.InvariantCulture)
    
    let escapeString (s : string) = 
        //if (s.Contains("\"")) then failwith "can't escape quotes"
        sprintf "\"%s\"" (s.Replace(@"\", @"\\").Replace("\"", "\\\""))
    
    let parseFlag (keyword : Flag) = 
        if (keyword.Contains("\"")) then failwith "can't escape quotes"
        if (keyword.Contains(" ")) then failwith "keyword should not contain spaces"
        keyword
    
    let rec parseQuery query = 
        match query with
        | SequenceSet(set) -> parseSequenceSet set
        | And(left, right) -> sprintf "%s %s" (parseQuery left) (parseQuery right)
        | All -> sprintf "ALL"
        | Answered -> sprintf "ANSWERED"
        | Bcc(bcc) -> sprintf "BCC %s" (escapeString bcc)
        | Before(date) -> sprintf "BEFORE %s" (parseDate date)
        | Body(body) -> sprintf "BODY %s" (escapeString body)
        | Cc(cc) -> sprintf "CC %s" (escapeString cc)
        | Deleted -> sprintf "DELETED"
        | Draft -> sprintf "DRAFT"
        | Flagged -> sprintf "FLAGGED"
        | From(from) -> sprintf "FROM %s" (escapeString from)
        | Header(header, content) -> sprintf "HEADER %s %s" (escapeString header) (escapeString content)
        | Keyword(flag) -> sprintf "KEYWORD %s" (parseFlag flag)
        | Larger(n) -> sprintf "LARGER %d" n
        | New -> sprintf "NEW"
        | Not(query) -> sprintf "NOT %s" (parseQuery query)
        | Old -> sprintf "OLD"
        | On(date) -> sprintf "ON %s" (parseDate date)
        | Or(queryLeft, queryRight) -> sprintf "OR %s %s" (parseQuery queryLeft) (parseQuery queryRight)
        | Recent -> sprintf "RECENT"
        | Seen -> sprintf "SEEN"
        | SentBefore(date) -> sprintf "SENTBEFORE %s" (parseDate date)
        | SentOn(date) -> sprintf "SENTON %s" (parseDate date)
        | SentSince(date) -> sprintf "SENTSINCE %s" (parseDate date)
        | Since(date) -> sprintf "SINCE %s" (parseDate date)
        | Smaller(n) -> sprintf "SMALLER %d" n
        | Subject(subject) -> sprintf "SUBJECT %s" (escapeString subject)
        | Text(text) -> sprintf "TEXT %s" (escapeString text)
        | To(toString) -> sprintf "TO %s" (escapeString toString)
        | Uid(set) -> sprintf "UID %s" (parseSequenceSet set)
        | Unanswered -> sprintf "UNANSWERED"
        | Undeleted -> sprintf "UNDELETED"
        | Undraft -> sprintf "UNDRAFT"
        | Unflagged -> sprintf "UNFLAGGED"
        | Unkeyword(flag) -> sprintf "UNKEYWORD %s" (parseFlag flag)
        | Unseen -> sprintf "UNSEEN"

/// This class represents any kind of finish from the other side
[<System.Serializable>]
type ImapException =     
    inherit System.Exception
    new (msg : string) = { inherit System.Exception(msg) }
    new (msg:string, inner:System.Exception) = { inherit System.Exception(msg, inner) }       
    new (info:System.Runtime.Serialization.SerializationInfo, context:System.Runtime.Serialization.StreamingContext) = {
        inherit System.Exception(info, context)
    }

type IImapWorker = 
    abstract SelectMailbox : string -> Async<FolderInfo>
    abstract Query : ImapQuery -> Async<AsyncSeq<Email>>
    abstract SetEmail : Email -> Async<unit>
    abstract DeleteEmail : Email -> Async<unit>
    abstract DeleteMailbox : unit -> Async<unit>
#if INTERACTIVE
    /// Debugging purposes
    abstract RawCommand : string -> Async<string>
#endif

type IMAPConnection = 
    | Connect of string * AsyncReplyChannel<exn option>
    | Authenticate of string * string * AsyncReplyChannel<exn option>
    /// Test if the connection is still up
    | Noop of AsyncReplyChannel<exn option>
    | DoWork of (IImapWorker -> Async<obj>) * AsyncReplyChannel<Choice<obj, exn>>
    | Disconnect of AsyncReplyChannel<exn option>

type IMAPProcessor = MailboxProcessor<IMAPConnection>

type SimpleWrapper(processor:IMAPProcessor, ?timeout) =
    let postAndAsyncReply f =
        async {
            let! (res:exn option) = processor.PostAndAsyncReply(f, ?timeout = timeout)
            if res.IsSome then
                reraisePreserveStackTrace res.Value
        }
    member x.Connect (server) = postAndAsyncReply (fun reply -> IMAPConnection.Connect(server, reply))
    member x.Disconnect () = postAndAsyncReply (fun reply -> IMAPConnection.Disconnect(reply))
    member x.Noop () = postAndAsyncReply (fun reply -> IMAPConnection.Noop(reply))
    member x.Authenticate (username, password) = postAndAsyncReply (fun reply -> IMAPConnection.Authenticate(username, password, reply))
    
    member x.DoWork(worker : IImapWorker -> Async<'a>) = 
        async {
            let! result = 
                processor.PostAndAsyncReply(
                    (fun reply -> IMAPConnection.DoWork((fun w -> async { let! res = worker w in return res :> obj }), reply)), 
                    ?timeout = timeout)
            return match result with
                   | Choice1Of2 res -> res :?> 'a
                   | Choice2Of2 exn -> raise exn
        }
    
type ImapConnection(processor : SimpleWrapper, server, username, password) = 
    let reconnect () = 
        async {
            do! processor.Connect server
            do! processor.Authenticate (username, password)
        }
    member x.Processor with get () = processor
    
    member x.DoWork(worker : IImapWorker -> Async<'a>) = 
        async { 
            try
                do! reconnect()
                return! processor.DoWork(worker)
            with
            | exn -> 
                Log.Err (fun _ -> L "Imap work failed, try to reconnect: %O" exn)
                do! processor.Disconnect()
                do! reconnect()
                Log.Err (fun _ -> L "Imap reconnect finished try to repeat work")
                return! processor.DoWork(worker)
        }
    
    member x.Query mailbox query = x.DoWork(fun w -> async { let! res = w.SelectMailbox mailbox
                                                             return! w.Query(query) })
    
    /// Sets the Email in the given Folder, 
    /// will either upload the Email or copy the existing email (when a MessageID is given)
    member x.SetEmail mailbox email = x.DoWork(fun w -> async { let! res = w.SelectMailbox mailbox
                                                                return! w.SetEmail(email) })
    
    /// Will delete the given Email from the mailbox
    member x.DeleteEmail mailbox email = x.DoWork(fun w -> async { let! res = w.SelectMailbox mailbox
                                                                   return! w.DeleteEmail(email) })

#if INTERACTIVE
    /// Debugging purposes
    member x.RawCommand cmd = x.DoWork(fun w -> async { return! w.RawCommand cmd })
#endif