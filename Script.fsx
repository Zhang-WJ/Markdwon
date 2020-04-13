type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>
    
and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan =
    | Literal of string
    | InlineCode of string
    | Strong of MarkdownSpans
    | Emphasis of MarkdownSpans
    | HyperLink of MarkdownSpans * string
    
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
    
