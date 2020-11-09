module Nessie.IR.Instruction

open Nessie.Expr


let bind: Type -> string -> Instruction = function
    | IntType -> BindNameInt
    | NumType -> BindNameNum
    | Any -> BindName
    | x -> failwithf "Cannot bind value of type %A to a name" x

let rec ofFunction (Function (argument, argType, body)): Instruction list = 
    [ bind argType argument ] @ ofExpr body @ [ UnbindName argument ]

and loadValue (v: Value): Instruction list =
    match v with
    | Int i -> [ LoadInt i ]
    | Num f -> [ LoadNum f ]
    | FunctionValue func ->
        ofFunction func
        |> Bytes.ofInstructions
        |> Bytes.sized
        |> Load
        |> List.singleton
    | Tuple values -> List.collect loadValue values

and ofExpr (expr: Expr) =
    match expr with
    | Const value -> loadValue value
    | Var (name, _) -> [ LoadName name ]
    | TupleExpr exprs -> List.collect ofExpr exprs
    | LambdaExpr (func, captured) -> 
        [Lambda (Seq.sort captured |> Array.ofSeq, ofFunction func |> Bytes.ofInstructions)]
    | Bind (name, nameType, toBind, body) ->
        ofExpr toBind @ [bind nameType name] @ ofExpr expr @ [UnbindName name]
    | LApply (func, arg) | RApply (arg, func) ->
        ofExpr func @ ofExpr arg @ [App]
    | TypeConvertion (e1, e2) -> failwith ""


