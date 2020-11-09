[<AutoOpen>]
module Nessie.Ast.Main

open ParLi.Linear


type AstType =
    /// A constant int value
    | ConstInt of int
    /// A constant number value
    | ConstNum of float
    /// A constant string value
    | ConstStr of string
    /// A variable term
    | VarNode of string
    /// Create a tuple
    | TupleNode of Ast list
    /// Create a lambda
    //| LambdaExpr of Function * closureVariables: int Set
    /// Convert the type 
    //| TypeConvertion of Expr * Type
    | BindNode of string * Ast * ret: Ast
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Ast * func: Ast
    /// Apply arg to func (but from the left to right)
    | LApply of func: Ast * arg: Ast
    /// Ambiguous application
    | Apply of Ast * Ast

and Ast = Ast of AstType * Range

