module Nessie.Expr.TypeResult

open System

let ok x = TypeResult(Some x, [])

let fail errs = TypeResult(None, errs)

let empty = TypeResult(None, [])

let ofOption opt = TypeResult (opt, [])

let errors (TypeResult (_, errs)) = errs

let appendErrors errs (TypeResult (x, errs')) = TypeResult(x, errs' @ errs)

let bind (f: 'a -> 'b TypeResult) (TypeResult (x, errs)) =
    match x with
    | Some x -> f x |> appendErrors errs
    | None -> fail errs

let map (f: 'a -> 'b) = bind (f >> ok)

let apply (TypeResult (x, xErrs)) (TypeResult (f, fErrs)) =
    let ``Option.apply`` f x =
        match f, x with
        | Some f, Some x -> Some(f x)
        | _ -> None

    TypeResult(``Option.apply`` f x, fErrs @ xErrs)

/// Map a Result producing function over a list to get a new Result
/// using applicative style
/// ('a -> Result<'b>) -> 'a list -> Result<'b list>
let rec traverse (f: 'a -> 'b TypeResult) list =

    // define a "cons" function
    let cons (head: 'c) tail = head :: tail

    // loop through the list
    match list with
    | [] ->
        // if empty, lift [] to a Result
        ok []
    | head :: tail ->
        // otherwise lift the head to a Result using f
        // and cons it with the lifted version of the remaining list
        map cons (f head) |> apply (traverse f tail)

//let sequence (s: Result<'a, 'err> seq)

type ResultBuilder() =
    member __.Return(x: 'a) = ok x

    member __.ReturnFrom(m: 'a TypeResult) = m

    member __.Bind(m: 'a TypeResult, f): 'b TypeResult = bind f m
    //member __.Bind((m, error): (Option<'T> * 'E), f) = m |> (ofOption >> appendErrors error) |> Result.bind f

    member __.Zero() = None

    member __.Combine(m: 'a TypeResult, f): 'b TypeResult = bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f ()

    member __.TryWith(m, h) =
        try
            __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try
            __.ReturnFrom(m)
        finally
            compensation ()

    member __.Using(res: #IDisposable, body): 'a TypeResult =
        __.TryFinally
            (body res,
             (fun () ->
                 match res with
                 | null -> ()
                 | disp -> disp.Dispose()))

    member __.While(guard, f) =
        if not (guard ()) then
            ok ()
        else
            do f () |> ignore
            __.While(guard, f)

    member __.For(sequence: seq<_>, body) =
        __.Using(sequence.GetEnumerator(), (fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current))))

let result = new ResultBuilder()
