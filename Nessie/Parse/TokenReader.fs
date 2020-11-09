module Nessie.Parse.TokenReader

open Nessie.Lex


let inline ofArray array = TokenReader(array, 0)

let inline get (TokenReader (tokens, i)) = tokens.[i]
let inline getType reader = get reader |> Token.tokenType

let inline advance amount (TokenReader (tokens, i)) =
    TokenReader(tokens, i + amount)

let inline pop (|Pattern|_|) reader =
    match get reader with
    | Pattern a -> Some(a, advance 1 reader)
    | _ -> None

let inline popType (|Pattern|_|) reader =
    match getType reader with
    | Pattern a -> Some(a, advance 1 reader)
    | _ -> None

let inline expect tokenType reader =
    popType (fun tokenType' -> if tokenType = tokenType' then Some() else None)
        reader
    |> Option.map snd

let inline (|Expect|_|) tokenType reader = expect tokenType reader

let inline (|ExpectWord|ExpectSymbol|ExpectInteger|ExpectNumber|ExpectString|ExpectConstType|) reader =
    let returnReader = advance 1 reader
    match get reader |> Token.tokenType with
    | Word name -> ExpectWord (name, returnReader)
    | Symbol name -> ExpectSymbol (name, returnReader)
    | Integer i -> ExpectInteger (i, returnReader)
    | Number n -> ExpectNumber (n, returnReader)
    | String s -> ExpectString (s, returnReader)
    | _ -> ExpectConstType returnReader
