// Learn more about F# at http://fsharp.org
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

    let four = Fourple("This", "is", "a", "fourple") //fourple of strings
    cmatch four



problem2()

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

type Result<'a> =
    |Success of 'a
    |Failure of String

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
    | []-> failwith "Premature termination of input" //message upon empty list
    | x::xs -> //matching the head token and piping them through
        match x with
        | IF -> xs |> eat ID |> eat THEN |> S |> eat ELSE |> S  //IF returns tail, expects ID, THEN, passes to S, expects else, passes to S
        | BEGIN -> xs |> S |> L //tail passes to S, passes to lookahead
        | PRINT -> xs |> eat ID //tail passes to S, eats ID then passes to S
        | EOF -> x::xs
        | _-> failwith (sprintf "wanted IF, BEGIN, PRINT, or EOF, got %A" x) //fail message

    //lookahead function 
    and L = function
    | [] -> failwith "Premature termination of input"
    | x::xs ->
        match x with
        | END -> xs
        | SEMICOLON -> xs |> S |> L
        |_-> failwith (sprintf "wanted END or SEMICOLON, got %A" x)


    let accept() = Success "Program Accepted." // successful program
    let error() = Failure "Program failed"

    //function from problem set
    let test_program program =
      let result = program |> S
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x = EOF then accept() else error()

    //first program 
    let tokens_prog1 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
    let prog1 = test_program tokens_prog1
    printfn("\nPROBLEM 3).")
    printfn("Program 1: %A") prog1

    //second program
    let tokens_prog2 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
    let prog2 = test_program tokens_prog2
    printfn("Program 2: %A") prog2

    //third program, causes error
    //let tokens_prog3 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
   // let prog3 = test_program tokens_prog3
   // printfn("Program 3: %A")prog3

problem3()

(*let problem4()=

    //make a syntax checker
    //Use suggestion in the notes to get around more than one lookahead
    //E -> E + T | E - T | T
    //T -> T * F | T / F | F
    //F -> i | (E)




problem4()*)

let problem5()=
    (*Write a curried F# function inner that takes two vectors represented as int list and returns their inner product.
        Throw an exception if the lists do not have the same length.
        Do not use any built-in or borrowed functions. Write it from scratch.
        Use big integers.
        Write a version without using tail recursion.
        Write another version using tail recursion.
        Try both versions on the input [1I..50000I] [50001I..100000I]. Increase the ranges until you get stack overflow on the non-tail-recursive version.*)

        //without tail recursion

        let x = [1;2;3]
        let y = [4;5;6]

        let rec inner a b =
            match (a,b) with
            | ([],[]) -> 0                                           //basecase if empty list
            | (_,[]) -> failwith ("Vectors are of different lengths") //basecase if x diff length
            | ([],_) -> failwith ("Vectors are of different lengths") //basecase if y diff length
            | (x::xs,y::ys) -> x * y + inner xs ys  //recursive call

        let nontail = inner x y
        printfn("\nPROBLEM 5).")
        printfn("Non-tail recursive product = %A") nontail


        let rec inner_tail a b acc =
            match (a,b) with
            | ([],[]) -> acc //tail recursion basecase where once there is an empty list, function returns the accumulator
            | (_,[]) -> failwith ("Vectors are of different lengths") //basecase if x is shorter
            | ([],_) -> failwith ("Vectors are of different lengths") //basecase if y is shorter
            | (x::xs,y::ys) -> inner_tail xs ys (acc + (x * y)) //the recursive call with the product being the accumulator

        //the accumulator will begin with 0
        let tail = inner_tail x y 0
        printfn("Tail recursive product = %A")tail
        

problem5()

let problem6()= 
(*            / 0 1 \
/ 1 2 3 \  *  | 3 2 |  =  /  9 11 \
\ 4 5 6 /     \ 1 2 /     \ 21 26 /
Write an uncurried F# function to do matrix multiplication:

  > multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;
    val it : int list list = [[9; 11]; [21; 26]]*)

    //transpose function from problem set 1
    let rec transpose xs =
        match xs with
        | [] -> xs
        | xs -> (List.map(fun x -> List.head x)xs)::transpose(List.map(fun x -> List.tail x)xs)

    //inner function from earlier problem
    let rec inner a b =
        match (a,b) with
        | ([],[]) -> 0
        | (x::xs,y::ys) -> (x * y) + inner xs ys
    
    let rec matrix xs =
        match xs with
        |(_,[])-> [] //return y despite x
        |([],_)-> [] 
        |(x::xs,ys) -> let trans_list = transpose ys
                       let inner_prod = List.map(fun y -> inner y x)trans_list
                       inner_prod::matrix(xs,ys)
    
    let list1 = ([[1;2;3];[4;5;6]])
    let list2 = ([[0;1];[3;2];[1;2]])

    printfn("PROBLEM 6).")
    //printfn("matrix multiply = %A") matrix(list1,list2)
    

problem6()

type Record = {Name: string; Credits: int; GPA: float}

let problem12()=
    let student = {Name = "Jones"; Credits=109; GPA= 3.85}
    
    printfn("\nPROBLEM 12).")
    printfn("Student = %A") student

problem12()



Console.ReadKey() |> ignore