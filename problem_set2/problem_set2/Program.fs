﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001



let problem1() =

    //TUPLE SIGNATURE: ('a * 'b) -> 'c
    //CURRIED SIGNATURE: ('a -> 'b -> 'c)

    //takes a curried function f x y and tuples them to f(x,y)
    let uncurry f = (fun x y -> f(x,y))
    // value: ('a * 'b -> c) -> ('a -> 'b -> 'c)

    //takes a uncurried function and curries them
    let curry f = (fun(x,y)->f x y)
    // value: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

    let plus = uncurry (+)


    printfn("PROBLEM 1:")
    printfn ("plus(2,3) = \n")




problem1()

type 'a Coordinates=
    | Tuple of x: 'a * y: 'a
    | Threeple of x: 'a * y: 'a * z: 'a
    | Fourple of x: 'a * y: 'a * z: 'a * w: 'a
    
let problem2()=

    printfn("PROBLEM 2:")

    let cmatch x =
        match x with
        | Tuple(x,y)-> printfn("Tuple(%A,%A)") x y
        | Threeple(x,y,z) -> printfn("Threeple(%A,%A,%A)") x y z
        | Fourple(x,y,z,w) -> printfn("Fourple(%A,%A,%A,%A)")x y z w

    let tup = Tuple(1,2) //tuple of ints
    cmatch tup

    let thr = Threeple(1.0,2.0,3.0) //threeple for floats 
    cmatch thr

    let four = Fourple("This", "is", "a", "fourple\n")
    cmatch four



problem2()

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let problem3()=


    //write a syntax checker

    //eat token function from lecture video
    let eat token = function
    | []-> failwith "Premature termination of input" //message displayed upon empty list
    | x::xs ->
        if x = token //if tokens match 
        then xs //then take token and return tail of list of tokens
        else failwith(sprintf "want %A, got %A" token x) //message displayed if they don't match

    //S function from lecture video
    let rec S = function
    | []-> failwith "Premature termination of input"
    | x::xs ->
        match x with
        | IF -> xs |> S |> eat ID |> eat THEN |> S |> eat ELSE |> S  //IF returns tail, expects ID, THEN, passes to S, expects else, passes to S
        | 


    let test_program program =
      let result = program |> S
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x = EOF then accept() else error()

problem3()

Console.ReadKey() |> ignore