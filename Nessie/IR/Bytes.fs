module Nessie.IR.Bytes

open System.IO
open Nessie.Expr


let empty = Bytes List.empty

let ofChunks = Bytes

let ofArray = List.singleton >> Bytes

let ofSeq (seq: #seq<_>) = seq |> Array.ofSeq |> ofArray


let toSeq (Bytes list) =
    match list with
    | [] -> Seq.empty
    | [ x ] -> x :> seq<_>
    | _ ->
        seq {
            for chunk in list do
                yield! chunk
        }

let toArray (Bytes list): byte array =
    match list with
    | [] -> Array.empty
    | [ x ] -> x
    | _ ->
        [| for chunk in list do
            yield! chunk |]


let append (Bytes left) (Bytes right) = left @ right |> ofChunks

let chunks (Bytes list) = list

let length = chunks >> List.sumBy Array.length

let flat = toArray >> ofArray


let ofInt (integer: int32): Bytes =
    let byte1 = byte <| (integer &&& 0x000000ff)
    let byte2 = byte <| (integer &&& 0x0000ff00) >>> 8
    let byte3 = byte <| (integer &&& 0x00ff0000) >>> 16
    let byte4 = byte <| (integer &&& 0xff000000) >>> 24
    ofArray [| byte1; byte2; byte3; byte4 |]

let ofByte (b: byte): Bytes = (ofInt(int32 b) |> toArray).[0..1] |> ofArray

let ofString (str: string): Bytes = Seq.map byte str |> ofSeq


let sized (bytes: Bytes) = append (length bytes |> ofInt) bytes


let ofInstructionCode (inst: InstructionCode): Bytes =
    let integer = int32 inst
    ofInt integer

let ofInstruction (inst: Instruction): Bytes =
    let code = InstructionCode.ofInstruction inst
    let header = ofInstructionCode code

    let content =
        let (@) = append

        match inst with
        | BindName name
        | BindNameInt name
        | BindNameNum name
        | UnbindName name
        | LoadName name -> ofString name |> sized
        | LoadInt i -> ofInt i
        | LoadNum _ -> failwith "lol no num yet"
        | Load bytes -> bytes
        | Add
        | App -> empty
        | Lambda (names, funcBytes) ->
            ofInt names.Length
            @ (names |> Seq.map (ofString >> sized) |> Seq.reduce (@))
            @ sized funcBytes

    append header content

let ofInstructions =
    List.fold (fun bytes inst -> append bytes (ofInstruction inst)) empty

let write (stream: Stream) (bytes: Bytes): unit Async = stream.AsyncWrite(toArray bytes)

let writeMany (stream: Stream) (bytesSeq: Bytes seq): unit Async =
    let asyncWrites = Seq.map (write stream) bytesSeq
    Async.Sequential asyncWrites |> Async.Ignore
