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
open Yaaf.Xmpp.MessageArchiveManager.IMAP.QueryParser

[<TestFixture>]
type ``Given the QueryParser``() = 
    let simpleQueries = [
        ImapQuery.All, "ALL"
        ImapQuery.Answered, "ANSWERED"
        ImapQuery.Bcc "test", "BCC \"test\""
        ImapQuery.Body "other test", "BODY \"other test\""
        ImapQuery.Cc "blub", "CC \"blub\""
        ImapQuery.Deleted, "DELETED";
        ImapQuery.Draft, "DRAFT"
        ImapQuery.Flagged, "FLAGGED"
        (ImapQuery.From "basd@asd.de"), "FROM \"basd@asd.de\""
        ImapQuery.Header ("headerkey", "headercontent"), "HEADER \"headerkey\" \"headercontent\""
        ImapQuery.Keyword "flag", "KEYWORD flag"
        ImapQuery.Larger 5, "LARGER 5"
        ImapQuery.New, "NEW"
        ImapQuery.Old, "OLD"
        ImapQuery.Recent, "RECENT"
        ImapQuery.Seen, "SEEN"
        ImapQuery.SequenceSet (1,5), "1:5"
        ImapQuery.Smaller 14, "SMALLER 14"
        ImapQuery.Subject "my subject", "SUBJECT \"my subject\""
        ImapQuery.Text "sample text", "TEXT \"sample text\""
        ImapQuery.To "same@blub.com", "TO \"same@blub.com\""
        ImapQuery.Uid (34,66), "UID 34:66"
        ImapQuery.Unanswered, "UNANSWERED"
        ImapQuery.Undeleted, "UNDELETED"
        ImapQuery.Undraft, "UNDRAFT"
        ImapQuery.Unflagged, "UNFLAGGED"
        ImapQuery.Unkeyword "flag", "UNKEYWORD flag"
        ImapQuery.Unseen, "UNSEEN"
    ]   
    let testDates = 
        [
            "10/25/1965", "\"25-Oct-1965\""
            "01/02/2018", "\"02-Jan-2018\""
            "01/01/2008", "\"01-Jan-2008\""
            "5/15/0001", "\"15-May-0001\""
            "12/31/2012", "\"31-Dec-2012\""
            "2/14/2012", "\"14-Feb-2012\""
            "1/1/0001", "\"01-Jan-0001\""
        ] 
        |> List.map (fun (raw, result) ->
             DateTime.Parse(raw, System.Globalization.CultureInfo.InvariantCulture), result)
             
    let dateQueries = [
        ImapQuery.Before, "BEFORE {0}" 
        ImapQuery.SentBefore, "SENTBEFORE {0}"
        ImapQuery.SentOn, "SENTON {0}"
        ImapQuery.SentSince, "SENTSINCE {0}"
        ImapQuery.On, "ON {0}"]    
        
    let filledDateQueries = 
        dateQueries
            |> Seq.collect
                (fun (rawQueryBuilder, rawResult) ->
                    testDates
                        |> Seq.map(fun (date, dateResult) -> rawQueryBuilder date, String.Format(rawResult, dateResult))) 
    
    let notQuery (query,result) =
        ImapQuery.Not query, sprintf "NOT %s" result
    
    let orQuery (query1,result1) (query2,result2)  =
        ImapQuery.Or (query1, query2), sprintf "OR %s %s" result1 result2
        
    let andQuery (query1,result1) (query2,result2)  =
        ImapQuery.And (query1, query2), sprintf "%s %s" result1 result2
    
    let level0 = simpleQueries |> Seq.append filledDateQueries |> Seq.cache
    let levelUp level =         
        let pairwise = 
            level
            |> Seq.pairwise
            |> Seq.cache
            
        let levelUpNot = 
            level
            |> Seq.map (fun (q) -> notQuery q)
            
        let levelUpOr = 
            pairwise
            |> Seq.map (fun (left, right) -> orQuery left right)
          
        let levelUpAnd = 
            pairwise
            |> Seq.map (fun (left, right) -> andQuery left right)
            
        levelUpNot |> Seq.append levelUpOr |> Seq.append levelUpAnd |> Seq.cache

        
    [<Test>]
    member this.
        ``Test if we can parse dates``  () = 
        testDates
            |> Seq.iter 
                (fun (date, result) ->
                    parseDate date |> should be (equal result))
        
    [<TestCase("2,4", Result="2:4")>]
    [<TestCase("1,10", Result="1:10")>]
    [<TestCase("4000,123120", Result="4000:123120")>]    
    [<Test>]
    member this.
        ``Test if we can parse seq sets``  (seqString:string) = 
        let [|left; right|] = seqString.Split(',') |> Array.map (fun s -> System.Int32.Parse s)
        parseSequenceSet (left, right)
        
    [<TestCase("4,2")>]
    [<TestCase("10,1")>]
    [<TestCase("123120,4000")>]    
    [<Test>]
    member this.
        ``Test if we can parse invalid seq sets``  (seqString:string) = 
        let [|left; right|] = seqString.Split(',') |> Array.map (fun s -> System.Int32.Parse s)
        (fun () -> parseSequenceSet (left, right) |> ignore) |> should throw typeof<System.Exception>
        
    [<TestCase("2,4", Result="\"2,4\"")>]
    [<TestCase("1,10", Result="\"1,10\"")>]
    [<TestCase("4000,123120", Result="\"4000,123120\"")>]
    [<TestCase("2/14/2012", Result="\"2/14/2012\"")>]
    [<TestCase("blub some more", Result="\"blub some more\"")>]    
    [<Test>]
    member this.
        ``Test if we can escape strings``  (toEscape:string) = 
        escapeString toEscape
        
    [<TestCase("2,4\"", Result="\"2,4\\\"\"")>]
    [<TestCase("1\",10", Result="\"1\\\",10\"")>]
    [<TestCase("4000\",123120", Result="\"4000\\\",123120\"")>]
    [<TestCase("2/14/2\"012", Result="\"2/14/2\\\"012\"")>]
    [<TestCase("blub \"some more\"", Result="\"blub \\\"some more\\\"\"")>]    
    [<TestCase("\\//", Result="\"\\\\//\"")>]    
    [<Test>]
    member this.
        ``Test if we can escape harder strings``  (toEscape:string) = 
        escapeString toEscape
    
    [<Test>]
    member this.
        ``Test if we can build simple queries`` () =          
        simpleQueries
            |> Seq.iter
            (fun (query, result)  ->
                parseQuery query |> should be (equal result))
        
    [<Test>]
    member this.
        ``Test if we can build queries with dates`` () =          
        filledDateQueries
            |> Seq.iter
            (fun (query, result)  ->
                parseQuery query |> should be (equal result))
    
    [<Test>]
    member this.
        ``Test if we can build advanced queries`` () =
        
        let maxLevel = 3
        let levels =
            [0..maxLevel]
                |> Seq.fold (fun oldLevel currentLevelIndex -> levelUp (List.head oldLevel) :: oldLevel) [level0]
        levels
            |> Seq.collect id
            |> Seq.iter
            (fun (query, result)  ->
                parseQuery query |> should be (equal result))
    
        
[<TestFixture>]
type ``Check the IMAP State machine``() = 
    
    [<Test>]
    member this.``Test if we can connect and reconnect`` () =
        let mem = MemoryImapProcessor.CreateProcessor()
        let con = ImapConnection(SimpleWrapper(mem), "test", "user", "pw")
        // See issue #7 (basically we want coverage of the imap state machine -> refactoring and adding unit tests)
        ()