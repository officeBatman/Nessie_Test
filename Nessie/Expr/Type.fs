module Nessie.Expr.Type

open Nessie.Expr.TypeResult

//let isSubtype (baseType: Type) (subType: Type): bool =
//    match baseType, subType with
//    | Any, _ -> true
//    | t, t' when t = t' -> true

let tryApply (argument: Type) (func: Type): TypeResult =
    match func with
    | FunctionType (input, output) when input = argument -> ok output
    | FunctionType (input, ouput) -> fail [ IncompatibleArgumentTypeError(input, argument) ]
    | _ -> fail [ InapplicableTypeError(func) ]


let rec ofFunction (Function (paramType, body)): TypeResult =
    result {
        let! bodyType = ofExpr body
        return FunctionType(paramType, bodyType)
    }

and ofValue (value: Value): TypeResult =
    match value with
    | Int _ -> ok IntType
    | Num _ -> ok NumType
    | FunctionValue func -> ofFunction func
    | Tuple values -> List.map ofValue values |> traverse id |> map NTupleType

and ofExpr (expr: Expr): TypeResult =
    match expr with
    | Const value -> ofValue value
    | Var (_, varType) -> ok varType
    | TypeConvertion (_, t) -> ok t
    | TupleExpr exprs -> List.map ofExpr exprs |> traverse id |> map NTupleType
    | LambdaExpr (func, _) -> ofFunction func
    | RApply (arg, func)
    | LApply (func, arg) ->
        result {
            let! argType = ofExpr arg
            let! funcType = ofExpr func
            return! tryApply argType funcType
        }
    | Bind (_, _, expr) -> ofExpr expr
