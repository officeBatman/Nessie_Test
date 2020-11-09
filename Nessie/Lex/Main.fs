[<AutoOpen>]
module Nessie.Lex.Main

open ParLi.Linear


/// A nessie-specific token type
[<Struct>]
type TokenType =
    | Word of word: string
    | Symbol of symbol: string
    | String of str: string
    | Number of num: float
    | Integer of int: int
    | RPipe
    | LPipe
    | Arrow
    | Let
    | LPara
    | RPara
    | LBrac
    | RBrac
    | LCurl
    | RCurl
    | Newline

/// A nessie-specific token.
/// It is aware of its range in the text file, it's indentation and it's type.
[<Struct>]
type Token = Token of Range * indentation: int * TokenType
