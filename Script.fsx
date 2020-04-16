type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>
    | BlockQuote of list<MarkdownBlock> // for quote Type in this DU
    
and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan =
    | Literal of string
    | InlineCode of string
    | Strong of MarkdownSpans
    | Emphasis of MarkdownSpans
    | HyperLink of MarkdownSpans * string
    | HardLineBreak
    
module Version_Explicit =
    let rec parseInlineBody acc = function
        | '`'::rest  ->
            Some(List.rev acc, rest)
        | c::chars ->
            parseInlineBody (c::acc) chars
        | [] -> None
        
    let parseInline = function
        | '`'::chars ->
            parseInlineBody [] chars
        | _ -> None
        
    // example for parseInline
    // "`code` and" |> List.ofSeq |> parseInline;;
    // output: val it:(char list * char list) = Some (['c'; 'o'; 'd'; 'e'], [' '; 'a'; 'n'; 'd'])
    
    // Helper function that coverts a list of characters to a string
    // This is needed when you want to return parsed characters
    let toString chars =
        System.String(chars |> Array.ofList)
        
    let rec parseSpans acc chars = seq {
        let emitLiteral = seq {
            if acc <> [] then
                yield acc |> List.rev |> toString |> Literal
        }
        
        match parseInline chars, chars with
        | Some(body, chars), _ ->
            yield! emitLiteral
            yield body |> toString |> InlineCode
            yield! parseSpans [] chars
        | _, c::chars ->
            yield! parseSpans (c::acc) chars
        | _, [] ->
            yield! emitLiteral
    }
    
module Version_ActivePattern =
    let toString chars =
        System.String(chars |> Array.ofList)
        
    let (|StartsWith|_|) prefix input =
        let rec loop = function
            | p::prefix, r::rest when p = r ->
                loop (prefix, rest)
            | [], rest ->
                Some rest
            | _ -> None
        loop (prefix, input)
    let rec parseBracketedBody closing acc = function
        | StartsWith closing (rest) ->
            Some (List.rev acc, rest)
        | c::chars ->
            parseBracketedBody closing (c::acc) chars
        | _ -> None
    let (|ParseBackted|_|) opening closing = function
        | StartsWith opening chars ->
            parseBracketedBody closing [] chars
        | _ -> None
        
    let (|Delimited|_|) delim = (|ParseBackted|_|) delim delim
    
    let rec parseSpans acc chars = seq {
        let emitLiteral = seq {
            if acc <> [] then
                yield acc |> List.rev |> toString |> Literal
        }
        
        match chars with
        | StartsWith [' '; ' '; '\n'; '\r'] chars
        | StartsWith [' '; ' '; '\n'] chars
        | StartsWith [' '; ' '; '\r'] chars ->
            yield! emitLiteral
            yield HardLineBreak
            yield! parseSpans [] chars
            
        | Delimited ['`'] (body, chars) ->
            yield! emitLiteral
            yield InlineCode (toString body)
            yield! parseSpans [] chars
        | Delimited ['*';'*'] (body, chars) ->
            yield! emitLiteral
            yield Strong (parseSpans [] body |> List.ofSeq)
            yield! parseSpans [] chars
        | Delimited ['*'] (body, chars)
        | Delimited ['_'] (body, chars) ->
            yield! emitLiteral
            yield Emphasis (parseSpans [] body |> List.ofSeq)
            yield! parseSpans [] chars
        | ParseBackted ['['] [']'] (linkBody, ParseBackted ['('] [')'] (url, chars)) ->
            yield! emitLiteral
            yield HyperLink (parseSpans [] linkBody |> List.ofSeq, toString url)
            yield! parseSpans [] chars
        
        | c::chars ->
            yield! parseSpans (c::acc) chars
        | [] ->
            yield! emitLiteral
    }
    
    //"**import `code`** and _emphasized_" |> List.ofSeq |> parseSpans [] |> List.ofSeq;;
    // below is output:  
    // val it : MarkdownSpan list =
    //  [Strong [Literal "import "; InlineCode "code"]; Literal " and ";
    //   Emphasis [Literal "emphasized"]]
    
    // "hello  \n\rworld  \r!!!"|> List.ofSeq |> parseSpans [] |> List.ofSeq;;
    // the output as below
    // val it : MarkdownSpan list =
    // [Literal "hello"; HardLineBreak; Literal "world"; HardLineBreak;
    // Literal "!!!"]
open Version_ActivePattern
module List =
    let partitionWhile f =
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let (|PrefixedLines|) (prefix: string) (lines:list<string>) =
    let prefixed, other =
        lines |> List.partitionWhile (fun (line:string) ->
           line.StartsWith(prefix))
    [ for (line: string )in prefixed ->
        line.Substring(prefix.Length) ], other
    
let (|LineSeparated|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    match List.partitionWhile (isWhite >> not) lines with
    | par, _::rest
    | par, ([] as rest) -> par, rest
    
let (|AsCharList|) (str: string) =
    List.ofSeq str

let (|ParseHeading|_|) lines =  
    match lines with
    | AsCharList (StartsWith ['#'; ' '] heading) :: lines ->
        Some(1, heading, lines)
        
    | AsCharList (StartsWith ['#'; '#'; ' '] heading) :: lines ->
        Some(2, heading, lines)
    
    | _ -> None

let (|ParseBlockQuote|_|) lines =
    match lines with
    | AsCharList (StartsWith ['>'] body) :: lines ->
        Some(body, lines)
        
    | _ -> None

let rec parseBlocks lines = seq {
    match lines with
    | ParseBlockQuote (body, lines) ->
        yield BlockQuote (parseBlocks ([toString body] |> List.ofSeq) |> List.ofSeq)
        yield! parseBlocks lines
    
    | ParseHeading (size, heading, lines) ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    
    | PrefixedLines "    " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines
        
    | LineSeparated (body, lines) when body <> [] ->
        let body = String.concat " " body |> List.ofSeq
        yield Paragraph(parseSpans [] body |> List.ofSeq)
        yield! parseBlocks lines
    
    | line:: lines when System.String.IsNullOrWhiteSpace(line) ->
        yield! parseBlocks lines
    
    | _ -> ()
}
    
let sample = """# Introducing F#
F# is a _functional-first_ language

>This is quote
which looks like this:

    let msg = "world"
    printfn "hello %s!" msg
This sample prints `hello world!`"""
// Sample for parseBlock
(*sample.Split('\r', '\n') |> List.ofSeq |> parseBlocks |> List.ofSeq;;
val it : MarkdownBlock list =
  [Heading (1,[Literal "Introducing F#"]);
   Paragraph
     [Literal "F# is a "; Emphasis [Literal "functional-first"];
      Literal " language which looks like this:"];
   CodeBlock ["let msg = "world""; "printfn "hello %s!" msg"];
   Paragraph [Literal "This sample prints "; InlineCode "hello world!"]]*)
   
   
open System.IO

let outputElement (output:TextWriter) tag attributes body =
    let attrString =
        [for k, v in attributes -> k + "=\"" + v + "\""]
        |> String.concat " "
        
    output.Write("<" + tag + attrString + ">")
    body()
    output.Write("</" + tag + ">")
    
let rec formatSpan (output: TextWriter) = function
    | Literal str ->
        output.Write(str)
    | Strong spans ->
        outputElement output "strong" [] (fun () ->
            spans |> List.iter (formatSpan output))
    | Emphasis spans ->
        outputElement output "em" [] (fun () ->
            spans |> List.iter (formatSpan output))
    | HyperLink (spans, url) ->
        outputElement output "a" ["href", url] (fun () ->
            spans |> List.iter (formatSpan output))
    | InlineCode code ->
        output.Write("<code>" + code + "</code>")

let rec formatBlock (output: TextWriter) =  function
    | Heading (size, spans) ->
        outputElement output ("h"+size.ToString()) [] (fun () ->
            spans |> List.iter (formatSpan output))
    | Paragraph spans ->
        outputElement output "p" [] (fun () ->
            spans |> List.iter (formatSpan output))
    | CodeBlock lines ->
        outputElement output "pre" [] (fun () ->
            lines |> List.iter output.WriteLine)
    | BlockQuote block ->
         outputElement output "blockquote" [] (fun () ->
            block |> List.iter (formatBlock output))