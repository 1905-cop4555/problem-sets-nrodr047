﻿open System
open System.Runtime.InteropServices
open System.Diagnostics

//problem set 1

let problem1()=
    printfn "1) ANSWER: C"
    printfn "  a). 2 + 5 * 10 = %d" (2 + 5 * 10)
    printfn "  b). 10I * 20I = %A" (10I + 20I)
    printfn "  c). 4 + 5.6 //cannot add float and int because they are different types."
    printfn "  d). Strings can be added = %s\n" ("4" + "5.6")

problem1()

let problem2()=
    printfn "2). ANSWER: C"
    printfn "  a). t1 * t2 -> t3 //is a tuple that returns something"
    printfn "  b). t1 -> t2 * t3 //is a function that returns a tuple"
    printfn "  c). t1 -> (t2 -> t3) //is the same as t1 -> t2 -> t3, which is curried form."
    printfn "  d). (t1 -> t2) -> t3 //is a function that returns something\n"

    let cadd a = fun b -> a + b
    
    printfn "   example:\n   cadd a = fun b -> a + b\n   (cadd 3) 7 = %A \n   int -> (int -> int)\n" ((cadd 3) 7)

problem2()

//let problem3()=

//problem3()

let problem4() = 
    printfn "4). ANSWER: B"
    printfn "Lists are immutable, they can be of any length, and are heterogenous and therefore, not polymorphic.\n"

problem4()

let problem5()=

    printfn "5). ANSWER: A"
    printfn "  a). 1::2::3::[] results in %A" (1::2::3::[])
    printfn "  b). 1@2@3@[] expects a list but gets an integer"
    printfn "  c). [1;2;3]::[] is interpreted as %A" ([1;2;3]::[])
    printfn "  d). ((1::2)::3)::[] expects an int list but gets an int\n"

problem5()

let problem6()=


    printfn "6). ANSWER: B"
    printfn " List.map List.head foo @ baz = value: list -> list -> list"
    printfn " ((List.map Listhead) foo) @ baz) = value: list -> list -> list\n"


problem6()

let problem7()= 

    printfn "7). ANSWER: D"
    printfn " int * bool -> string list is interpreted as a tuple that returns a string list"
    printfn " therefore, (int * bool) -> (string list) is correct.\n"

problem7()

let problem8()= 

    let rec foo = function
        |(xs, []) ->  xs //if tuple only contains first list, return first list
        |(xs, y::ys)-> foo (xs@[y], ys) //else call foo and append y to the end of xs until ys is empty.

    let xs = [1;2;3]
    let ys = [4;5;6]

    printfn "8). ANSWER: D"
    printfn "    Recursive rules:"
    printfn "    1. Make sure the basecases return the correct answers."
    printfn "    2. Assume the recursive call works."
    printfn "    3. Make sure each recursive call works on smaller input."
    printfn "    4. Make sure there are enough cases for all inputs"
    printfn " let xs = %A" (xs)
    printfn " let ys = %A" (ys)
    printfn "  foo (xs,ys) = %A" (foo (xs,ys))
    printfn "Therefore, the recursive function foo works correctly and does all items in the checklist\n"
    


problem8()

let problem9()=

    printfn "9). ANSWER: C"
    printfn "   fun f -> f 17 = value : (int -> 'a ) -> 'a\n"
    //left associative. int 17 goes into function f, f returns a value ('a) that inputs into function f and returns 'a.
    //fun f -> f 17



problem9()

let problem10()=

    printfn "10). ANSWER: D"
    printfn "   a). (@)[5] value: int list -> int list"
    printfn "   b). [fun x -> x + 1] value: ('a ^ 'b) list -> ('a ^ 'b) list -- error" 
    printfn "   c). fun x -> 5::x value: int list" //notice 5 is just an int
    printfn "   d). fun x -> x::[5] value: int -> int list" // int x appends to an int list

problem10()


Console.ReadKey() |> ignore
