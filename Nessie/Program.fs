// Learn more about F# at http://fsharp.org

open System
open System.IO

open ParLi.Parsers
open ParLi.Text

open Nessie.IR
open Nessie.IR.Serialize

open Nessie.Lex

[<EntryPoint>]
let main argv =
    let ir: Instruction list = [
        LoadInt 2
        LoadInt 3
        Add
    ]

    let byteArray = using (new MemoryStream()) (fun stream -> 
        Async.RunSynchronously (serialize stream ir)
        stream.ToArray ())

    printfn "Instructions:"
    printfn "%A" ir
    printfn "serialized to"
    printfn "%A" byteArray

    printf "\n\n\n"

    let text = "let positive = x -> x bigger-than 0"
    let inp = Input.ofString text
    let result = 
        match parseWith lex inp [] |> ParseResult.toResult with
        | Error (_, err) -> 
            [for (p, e) in err do
                sprintf "ERROR IN POSITION %O: %s" p e]
            |> String.concat "\n"
        | Ok tokens ->
            sprintf "%A" tokens
    
    printfn "Lexing program:"
    printfn "%A" text
    printfn "Result:"
    printfn "%s" result

    0 // return an integer exit code
