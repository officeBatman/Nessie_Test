[<AutoOpen>]
module Nessie.IR.Data

open Nessie.Expr


type Bytes = Bytes of byte array list   //  Stored as a list of chunks

type InstructionCode =
    | Bind = 0uy
    | BindInt = 1uy
    | BindNum = 2uy
    | LoadName = 10uy
    | LoadVar = 11uy
    | LoadInt = 12uy
    | LoadNum = 13uy
    | Add = 20uy
    | Sub = 21uy
    | Mul = 22uy
    | Div = 23uy
    | Mod = 24uy
    | Abs = 25uy
    | Neg = 26uy
    | Inc = 27uy
    | Dec = 28uy
    | App = 29uy
    | Lambda = 30uy

type Instruction =
    | BindNameInt of string 
    | BindNameNum of string 
    | BindName of string 
    | UnbindName of string
    | LoadName of string
    | LoadInt of int
    | LoadNum of float
    | Load of Bytes
    | Add
    | App
    | Lambda of string array * Bytes

//type IR = IR of Instruction list

