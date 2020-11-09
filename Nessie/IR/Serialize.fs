module Nessie.IR.Serialize

open Nessie.IR
open System
open System.IO


let instructionCodeBytes (inst: InstructionCode): Bytes =
    let b = byte inst
    Bytes.ofByte b

let instructionBytes (inst: Instruction): Bytes =
    let code = InstructionCode.ofInstruction inst
    let header = instructionCodeBytes code

    let content =
        match inst with
        | BindName name
        | LoadName name -> Bytes.ofString name |> Bytes.sized
        | Add -> Bytes.empty
        | LoadInt i -> Bytes.ofInt i
        | LoadNum _ -> failwith "lol no num yet"
        | Load bytes -> bytes

    Bytes.append header content

let writeBytes (stream: Stream) (bytes: Bytes): unit Async =
    let asyncWrites = Bytes.chunks bytes |> Seq.map stream.AsyncWrite
    Async.Sequential asyncWrites |> Async.Ignore

let serialize (stream: Stream) (ir: Instruction list): unit Async =
    let asyncWrites = Seq.map (instructionBytes >> writeBytes stream) ir
    Async.Sequential asyncWrites |> Async.Ignore

//let serialize (stream: Stream) (IR operations): unit Async =
//    let asyncWrites = Seq.map (operationBytes >> writeBytes stream) operations
//    Async.Sequential asyncWrites |> Async.Ignore
