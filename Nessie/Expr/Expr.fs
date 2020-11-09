/// Contains the Expr data type - For representing unambiguous, type checked, programs that can be translated to IR
[<AutoOpen>]
module Nessie.Expr.Main

type Function = Function of Type * Expr

and Value =
    | Int of int
    | Num of float
    | Str of string
    | FunctionValue of Function
    //| Lambda of Function * closure: Map<string, Value>
    | Tuple of Value list

/// A type is a set of value's, sometimes represented by a predicate (which can be compiled)
and Type = 
    | Any
    | IntType 
    | NumType
    | FunctionType of Type * Type
    | NTupleType of Type list
    | XTupleType of Type
    | Predicate of Function
    | Specific of Value

and Expr = 
    /// A constant value
    | Const of Value
    /// Load the value of a variable 
    | Var of int * Type
    /// Create a tuple
    | TupleExpr of Expr list
    /// Create a lambda
    | LambdaExpr of Function * closureVariables: int Set
    /// Convert the type 
    | TypeConvertion of Expr * Type
    | Bind of Type * Expr * Expr
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Expr * func: Expr
    /// Apply arg to func (but from the left to right)
    | LApply of func: Expr * arg: Expr


type TypeError = 
    | InapplicableTypeError of Type
    | IncompatibleArgumentTypeError of Type * Type
    | UnconvertableType of Type * Type

type 'a TypeResult = TypeResult of 'a option * TypeError list

type TypeResult = Type TypeResult
