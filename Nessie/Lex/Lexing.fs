[<AutoOpen>]
module Nessie.Lex.Lexing

open System.Text.RegularExpressions

open ParLi.Parsers
open ParLi.Linear
open ParLi.Text


//let private tokenRegexs =
//    let inline ret x _ = x
//    let inline mapFst mapping (a, b) = (mapping a, b)

//    seq {
//        @"^\w[\w\-!?']*[\w']|\w", Word
//        @"^[!.=#<>@$%?&`:*|+,\-;\/\\~^]+", Symbol
//        @"^""[^""]*""", String
//        @"^\d+(\.\d*)?|\.\d+", float >> Number
//        @"^0[bB][01_]+|0[xX][0-9a-fA-F_]+|\d+", int >> Integer
//        @"^>", ret RPipe
//        @"^<", ret LPipe
//        @"^(", ret LPara
//        @"^)", ret RPara
//        @"^[", ret LBrac
//        @"^]", ret RBrac
//        @"^{", ret LCurl
//        @"^}", ret RCurl
//    }
//    |> Seq.map
//        (mapFst
//         <| fun pattern -> new Regex(pattern, RegexOptions.Compiled))
//    |> Array.ofSeq

let tokenType =
    let ret x _ = x

    let rangeAndMap mapping parser =
        tuple3 position (parser |>> mapping) position
        |>> fun (startPos, ret, endPos) ->
                Range.ofPositions startPos endPos, ret

    choice [ regex @"^\w[\w\-!?']*[\w']|\w" |> rangeAndMap Word
             regex @"^[!.=#<>@$%?&`:*|+,\-;\/\\~^]+"
             |> rangeAndMap Symbol
             regex @"^""[^""]*""" |> rangeAndMap String
             regex @"^\d+(\.\d*)?|\.\d+" |> rangeAndMap (float >> Number)
             regex @"^0[bB][01_]+|0[xX][0-9a-fA-F_]+|\d+"
             |> rangeAndMap (int >> Integer)
             regex @"^>" |> rangeAndMap (ret RPipe)
             regex @"^<" |> rangeAndMap (ret LPipe)
             regex @"^(" |> rangeAndMap (ret LPara)
             regex @"^)" |> rangeAndMap (ret RPara)
             regex @"^[" |> rangeAndMap (ret LBrac)
             regex @"^]" |> rangeAndMap (ret RBrac)
             regex @"^{" |> rangeAndMap (ret LCurl)
             regex @"^}" |> rangeAndMap (ret RCurl) ]

//let lexTokenType (Text (str, indent, Position i) as inp): Token option * Text =
//    tokenRegexs
//    |> Array.tryPick (fun (regex, tokenTypeFunction) ->
//        let matchObj = regex.Match(str.[i..])

//        if matchObj.Success then
//            let substr = matchObj.Value
//            let tokenType = tokenTypeFunction substr

//            Some
//                (Token.token substr.Length inp tokenType,
//                 Text.advance substr.Length inp)
//        else
//            None)
//    |> function
//    | Some (token, text) -> Some token, text
//    | None ->
//        //  try to guess the end of the invalid token
//        let mutable guess = str.IndexOf(' ', i)
//        if guess = -1 then guess <- str.Length - 1
//        None, Text.advance (guess - i) inp

//let advanceSpaces (Text (str, indent, pos) as text): Text =
//    let maxPos = Text.maxPosition text
//    let mutable pos = pos
//    let mutable indent = indent

//    while pos <= maxPos
//          && System.Char.IsWhiteSpace(str, (Position.index pos)) do
//        if str.[Position.index pos] = '\t' then indent <- indent + 1
//        pos <- Position.advance 1 pos

//    Text(str, indent, pos)

let tabs = many (charSkip '\t' <|> stringSkip "    ") |>> List.length

let failwith message = 
    position >>= fun pos -> Parser.updateState ((@) [pos, message]) >>. Parser.failFatally

let lexLine =
    tabs .>>. (many (spaces >>. tokenType <|> failwith "Invalid token"))
    .>> spaces
    .>> many newline
    |>> fun (indent, tokens) ->
            List.map (fun (range, tokenType) ->
                Token.token (range, indent, tokenType)) tokens

let lex: Parser<Token array, Input, list<Position * string>> =
    many lexLine |>> (List.concat >> Array.ofSeq)

//let lexString str: Result<Token array, Range list> =
//    let mutable text = Text.ofString str
//    let mutable reversedResult = []
//    let mutable reversedErrors = []

//    text <- advanceSpaces text

//    while Text.inRange text do
//        let startPosition = Text.position text

//        let maybeToken, text' = lexTokenType text
//        text <- text'

//        match maybeToken with
//        | Some token -> reversedResult <- token :: reversedResult
//        | None ->
//            let endPosition = Text.position text
//            let range = Range.ofPositions startPosition endPosition
//            reversedErrors <- range :: reversedErrors

//        text <- advanceSpaces text

//    if reversedErrors.IsEmpty then
//        Ok(Array.ofSeq <| Seq.rev reversedResult)
//    else
//        Error(List.rev reversedErrors)
