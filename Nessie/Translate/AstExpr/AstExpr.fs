[<AutoOpen>]
module Nessie.Translate.AstExpr.Main

open ParLi.Parsers
//open ParLi.Trees

open Nessie.Ast
open Nessie.Expr


type Context = { vars: list<string * Type> }
let (|Context|) ({ vars = vars }: Context) = vars

module Context =
    /// Return the var id (index in the list) of a variable by name
    let var name (Context vars) = 
        List.tryFindIndex (fst >> (=) name) vars
        |> Option.map (fun i -> i, snd vars.[i])


type ConvertionErrorType = 
    | UnknownVariable of string

[<Struct>]
type ConvertionError = ConvertionError of ConvertionErrorType * ParLi.Linear.Main.Range

type 'a Result = Result<'a, ConvertionError list>

module private Result =
    let inline ofOption error option = 
        match option with
        | Some ok -> Ok ok
        | None -> Error error

    let inline ofOptionWith (error: unit -> _) option = 
        match option with
        | Some ok -> Ok ok
        | None -> Error (error ())

    let inline ofOptionLazy (error: Lazy<_>) option = 
        match option with
        | Some ok -> Ok ok
        | None -> Error error.Value

//let rec translate (context: Context) (ast: Ast): Expr Result =
//    match ast with
//    | ConstInt i -> Ok <| Const (Int i)
//    | ConstNum n -> Ok <| Const (Num n)
//    | ConstStr s -> Ok <| Const (Str s)
//    | VarNode name -> 
//        Context.var context name
//        |> Option.map Var
//        |> Result.ofOptionLazy (lazy (ConvertionError(UnknownVariable name, ))
//    | TupleNode asts -> 
//        let rec collect (results: #seq<'a Result>): 'a list Result =
//            let mutable ret = Ok []
//            results |> Seq.iter (function
//                | Ok res -> 
//                    ret |> Result.map (fun x ->
//                        ret <- Ok (x @ [res]))
//                    |> ignore
//                | Error errors -> 
//                    match ret with
//                    | Ok _ ->
//                        ret <- Error errors
//                    | Error moreErrors ->
//                        ret <- Error (moreErrors @ errors))
//            ret

//        List.map (translate context) asts 
//        |> collect
//        |> Result.map TupleExpr
//    | Ast.LApply (func, arg) ->
//        failwith ""

open ParLi.Linear

let rec translate: Parser<_, Ast, _> =
    Parser.input >>= (Ast.astType >> function
        | ConstInt i -> ret <| Const (Int i)
        | ConstNum n -> ret <| Const (Num n)
        | ConstStr s -> ret <| Const (Str s)
        | VarNode name -> 
            Parser.state 
            |> Parser.choose (Context.var name >> Option.map Var)
        | TupleNode asts -> 
            let inp = Input.ofValue asts
            many (pop() >>=  translate) 
            |> parseWith
            <| inp <| [])

let rec translate (context: Context) (ast: Ast): Expr Result =
    match ast with
    | ConstInt i -> Ok <| Const (Int i)
    | ConstNum n -> Ok <| Const (Num n)
    | ConstStr s -> Ok <| Const (Str s)
    | VarNode name -> 
        Context.var context name
        |> Option.map Var
        |> Result.ofOptionLazy (lazy (ConvertionError(UnknownVariable name, ))
    | TupleNode asts -> 
        let rec collect (results: #seq<'a Result>): 'a list Result =
            let mutable ret = Ok []
            results |> Seq.iter (function
                | Ok res -> 
                    ret |> Result.map (fun x ->
                        ret <- Ok (x @ [res]))
                    |> ignore
                | Error errors -> 
                    match ret with
                    | Ok _ ->
                        ret <- Error errors
                    | Error moreErrors ->
                        ret <- Error (moreErrors @ errors))
            ret

        List.map (translate context) asts 
        |> collect
        |> Result.map TupleExpr
    | Ast.LApply (func, arg) ->
        failwith ""



    
