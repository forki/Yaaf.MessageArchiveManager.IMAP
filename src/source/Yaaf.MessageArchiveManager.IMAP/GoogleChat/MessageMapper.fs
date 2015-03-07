// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.MessageArchiveManager.IMAP.GoogleChat

open Yaaf.Xmpp.MessageArchiveManager
open Yaaf.Xmpp.MessageArchiveManager.IMAP
open ChatParser
open Yaaf.Logging

/// A simple implementation of the IMessageMapper by mapping emails exactly the way google chat does.
module MessageMapper = 
    let noteIdentifier = "NOTE@notes.xmpp.yaaf.de"
    
    let convertChatMessageToChatItem (archiveOwner : JabberId) (chatMessage : ChatMessage) = 
        let chatContent : Yaaf.Xmpp.MessageArchiving.ChatContent = 
            { Body = Some chatMessage.Body
              AdditionalData = [] // Save the additional data in there?
                                  }
        
        let isNote = chatMessage.From = noteIdentifier
        
        let from = 
            if isNote then None
            else Some <| JabberId.Parse chatMessage.From
        
        let messageInfo : Yaaf.Xmpp.MessageArchiving.MessageInfo = 
            { Sec = None
              Name = None
              Jid = from
              Utc = 
                  chatMessage.TimestampReceived
                  |> convertUnixTimestamptoDateTime
                  |> Some }
        
        if isNote then Yaaf.Xmpp.MessageArchiving.ChatItem.Note(chatMessage.Body, messageInfo.Utc)
        elif from.Value.BareId = archiveOwner.BareId then Yaaf.Xmpp.MessageArchiving.ChatItem.To(messageInfo, chatContent)
        else Yaaf.Xmpp.MessageArchiving.ChatItem.From(messageInfo, chatContent)
    
    let convertChatItemToChatMessage (archiveOwner : JabberId) (other : JabberId) (chatItem : Yaaf.Xmpp.MessageArchiving.ChatItem) = 
        let isTo = 
            match chatItem with
            | Yaaf.Xmpp.MessageArchiving.ChatItem.Note(body, utc) -> false
            | Yaaf.Xmpp.MessageArchiving.ChatItem.To(messageInfo, chatContent) -> true
            | Yaaf.Xmpp.MessageArchiving.ChatItem.From(messageInfo, chatContent) -> false
        match chatItem with
        | Yaaf.Xmpp.MessageArchiving.ChatItem.Note(body, utc) -> 
            { /// The Target of the message
              To = archiveOwner.FullId
              /// The author of the message
              From = noteIdentifier
              /// Set to -1 to tell the parser to not set this attribute
              SequenceNo = -1
              /// Set to -1 to tell the parser to not set this attribute
              LogSource = -1
              /// Set to 0 to tell the parser to not set this attribute
              Cid = 0UL
              /// A unix Timestamp in ms
              TimestampServer = (utc.Value |> convertDateTimeToUnixTimestamp) - 60L
              /// The Chat Message
              Body = body
              /// A unix Timestamp in ms
              TimestampReceived = utc.Value |> convertDateTimeToUnixTimestamp
              CliAsDefault = false }
        | Yaaf.Xmpp.MessageArchiving.ChatItem.To(messageInfo, chatContent) | Yaaf.Xmpp.MessageArchiving.ChatItem.From(messageInfo, chatContent) -> 
            let cliAsDefault, receiver, sender = 
                if isTo then 
                    // We are the author so the message is obviously sent to "With" 
                    true, 
                    (if messageInfo.Jid.IsNone then other
                     else messageInfo.Jid.Value), archiveOwner
                else 
                    false, 
                    // We are not the Author so either we are in a group chat then 
                    // the message is sent to the group "With"
                    // or we have a private chat then the receiver is "Self"
                    // private chat test
                    archiveOwner, 
                    (if messageInfo.Jid.IsNone then other
                     else messageInfo.Jid.Value)
            { /// The Target of the message
              To = receiver.FullId
              /// The author of the message
              From = sender.FullId
              /// Set to -1 to tell the parser to not set this attribute
              SequenceNo = -1
              /// Set to -1 to tell the parser to not set this attribute
              LogSource = -1
              /// Set to 0 to tell the parser to not set this attribute
              Cid = 0UL
              /// A unix Timestamp in ms
              TimestampServer = (messageInfo.Utc.Value |> convertDateTimeToUnixTimestamp) - 60L
              /// The Chat Message
              Body = chatContent.Body.Value
              /// A unix Timestamp in ms
              TimestampReceived = messageInfo.Utc.Value |> convertDateTimeToUnixTimestamp
              CliAsDefault = cliAsDefault }
    
    let toJabberId (mailAddress : MailAddress) = JabberId.Parse mailAddress.Address
    
    let getUtc (item : Yaaf.Xmpp.MessageArchiving.ChatItem) = 
        match item with
        | Yaaf.Xmpp.MessageArchiving.ChatItem.Note(body, utc) -> utc.Value
        | Yaaf.Xmpp.MessageArchiving.ChatItem.To(messageInfo, _) | Yaaf.Xmpp.MessageArchiving.ChatItem.From(messageInfo, _) -> messageInfo.Utc.Value
    
    /// parse the given Email
    let convertEmailToCollection (parser : IChatParser) (email : Email) : ParsedCollection = 
        let maybeChat = 
            match email.XmlContent with
            | None -> failwith "No chat email given!"
            | Some chatXml -> chatXml
        
        // archive owner
        let toJid = toJabberId (email.To |> Seq.exactlyOne)
        // other entity
        let fromJid = toJabberId (email.From |> Seq.exactlyOne)
        let conv = parser.ParseXml maybeChat
        
        let id : Yaaf.Xmpp.MessageArchiving.ChatCollectionId = 
            { With = fromJid
              Start = email.Date }
        if conv.IsRemoved then 
            DeletedCollection({ Id = id
                                Version = conv.Version } : Yaaf.Xmpp.MessageArchiving.ChangeItemData)
        else 
            let simpleMessages = 
                conv.Messages
                |> Seq.map (convertChatMessageToChatItem toJid)
                |> Seq.cache
            
            // We need the start date as the email is dated to the last message
            let firstMsg = simpleMessages |> Seq.head
            let firstMsgUtc = getUtc firstMsg
            
            let header : Yaaf.Xmpp.MessageArchiving.ChatCollectionHeader = 
                { Id = id
                  Thread = Some email.MessageID
                  Subject = Some email.Subject
                  Version = Some conv.Version }
            NormalCollection({ Header = header
                               ChatItems = simpleMessages |> Seq.toList
                               SetPrevious = false
                               Previous = None
                               SetNext = false
                               Next = None } : Yaaf.Xmpp.MessageArchiving.ChatCollection)
    
    let tryConvertEmailToCollection (parser : IChatParser) email = 
        match email.XmlContent with
        | None -> None
        | Some chatXml -> 
            match parser.IsGoogleChat chatXml with
            | true -> 
                try 
                    convertEmailToCollection parser email |> Some
                with
                    | ChatParserException msg -> 
                        Log.Err(fun _ -> L "unable to parse message: %O" msg)
                        None
                    | ex -> 
                        Log.Crit(fun _ -> L "parsing message %O failed with unknown exception %O" chatXml ex)
                        reraise()
            | false -> None
    
    let generateHtmlContent (owner : JabberId) (col : ParsedCollection) = 
        match col with
        | DeletedCollection _ -> "<b>This message archives an xmpp conversation, you should not simply delete or change it!</b>"
        | NormalCollection col -> 
            let enc = System.Web.HttpUtility.HtmlEncode
            let builder = new System.Text.StringBuilder()
            builder.AppendFormat("""<html xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <title>Conversation from {0}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    </head>
    <body style="background-color:gainsboro;">
        <div style="background-color:lightcoral; text-align:center;  padding:5px;">
            <b>This message archives an xmpp conversation, you should not simply delete or change it!</b>
        </div>""", col.Header.Id.Start) |> ignore
            for item in col.ChatItems do
                let from = 
                    if item.IsToItem then owner
                    else col.Header.Id.With
                
                let dateString = 
                    match item.Date with
                    | Some d -> System.String.Format("<small>{0}</small>", enc (System.String.Format("<{0}>", d)))
                    | None -> "<small>unknown</small>"
                
                builder.AppendFormat("""<div style="background-color:{4}; padding:5px;">
            <b>{0} {3}:</b><br/>
            {1}
        </div>
        <div style="margin:5px 5px 5px 30px;">
            {2}
        </div>""", enc (from.BareId), dateString, enc item.Message, 
                                     (if item.IsNoteItem then " (NOTE)"
                                      else ""), 
                                     if item.IsToItem then "lightblue"
                                     else "lightgreen")
                |> ignore
            builder.AppendLine("""
    </body></html>""") |> ignore
            builder.ToString()
    
    let convertMessageCollectionToEmail (parser : IChatParser) (owner : JabberId) (col : ParsedCollection) : Email = 
        let id = col.Id
        
        let messageId, subject, colData = 
            match col with
            | NormalCollection col -> 
                let simpleMessages = col.ChatItems
                let last = simpleMessages |> Seq.last
                let chatMessages = simpleMessages |> Seq.map (convertChatItemToChatMessage owner col.Header.Id.With)
                col.Header.Thread.Value, col.Header.Subject.Value, 
                { Messages = chatMessages
                  Version = 
                      match col.Header.Version with
                      | Some v -> v
                      | None -> 0
                  IsRemoved = false }
            | DeletedCollection id -> 
                "This collection was deleted", "This collection was deleted", 
                { Messages = Seq.empty
                  Version = id.Version
                  IsRemoved = true }
        
        let chatXml = parser.WriteXml colData
        { ReceivedDate = System.DateTime.Now
          SequenceNumber = None
          Date = id.Start
          Subject = subject
          MessageID = messageId
          UID = None
          To = 
              [ { Name = ""
                  Address = owner.BareId } ]
          From = 
              [ { Name = ""
                  Address = id.With.BareId } ]
          XmlContent = Some chatXml // This is the Element we need
          /// The HTML Content of the Message
          HtmlContent = generateHtmlContent owner col
          /// The Text Content of the Message
          TextContent = "This message archives an xmpp conversation, you should not simply delete or change it!" }
    
    let createMessageMapper (owner : JabberId) = 
        let parser = ChatParser.createParser()
        { new IMessageMapper with
              member x.ConvertEmailToMessageCollection email = tryConvertEmailToCollection parser email
              member x.ConvertMessageCollectionToEmail col = convertMessageCollectionToEmail parser owner col }
