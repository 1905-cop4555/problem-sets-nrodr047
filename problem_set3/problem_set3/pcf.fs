﻿module pcf

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
    | 
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp

