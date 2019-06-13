module pcf

// Skeleton file for PCF interpreter

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open parser.Parse

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
| NUM n -> NUM n
| BOOL b -> BOOL b
| SUCC -> SUCC
| PRED -> PRED
| ISZERO -> ISZERO
| ERROR s -> ERROR s
| IF (e1,e2,e3) -> 
    match (interp e1) with
    | BOOL true -> interp e2
    | BOOL false -> interp e3
    | v -> ERROR (sprintf "expecting bool %A" v)
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (ISZERO, NUM 0) -> BOOL true
    | (ISZERO, NUM n) -> BOOL false
    | (ISZERO, v) -> ERROR (sprintf "'iszero' need int argument, not %A" v)
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM 0) -> NUM 0
    | (PRED, NUM n) -> NUM (n-1)
    | (PRED, v) -> ERROR (sprintf "'pred' needs int argument, not %A " v)
    | (v1, v2) -> ERROR (sprintf "expected term, got %A,%A" v1 v2)


// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp

open System
let programs = 
    printfn "succ 0 = %A" <| interpstr "succ 0"
    printfn "succ 1 = %A" <| interpstr "succ 1"
    printfn "pred 0 = %A" <| interpstr "pred 0"
    printfn "succ(succ(succ 0)) = %A" <| interpstr "succ(succ(succ 0))"
    printfn "iszero succ = %A" <| interpstr "iszero succ" //error
    printfn "succ pred 7 = %A" <| interpstr "succ pred 7" //error


programs

Console.ReadKey() |> ignore

