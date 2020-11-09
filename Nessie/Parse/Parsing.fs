[<AutoOpen>]
module Nessie.Parse.Parsing

open Nessie.Lex
open Nessie.Ast

open ParLi.Parsers
open ParLi.Linear
open ParLi.Arrays


let expri, expr = Parser.ref ()

let is t = popChoose (fun x -> if Token.tokenType x = t then Some t else None)

let atom = 
    choice [
        is LPara >>. expected "Expected expression after '('" expr .>> expected "Unclosed parentheses" (is RPara)
        popChoose (Token.tokenType >> function Word name | Symbol name -> Some (VarNode name) | _ -> None)
        popChoose (Token.tokenType >> function Integer i -> Some (ConstInt i) | _ -> None)
        popChoose (Token.tokenType >> function Number n -> Some (ConstNum n) | _ -> None)
        popChoose (Token.tokenType >> function String s -> Some (ConstStr s) | _ -> None)
    ]

let pipeOpt = choice [ is LPipe >>. ret LApply
                       is RPipe >>. ret RApply
                       ret Apply ]

do expri (
    atom .>>. many (pipeOpt .>>. atom)
    |>> fun (x, xs) ->
        List.fold (fun x (f, y) -> f (x, y)) x xs)
