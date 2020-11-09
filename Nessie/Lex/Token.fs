module Nessie.Lex.Token

open ParLi.Linear


let token (range, indent, tokenType) = Token (range, indent, tokenType)

let range (Token (range, _, _)) = range

let indent (Token (_, i, _)) = i

let tokenType (Token (_,_,t)) = t
