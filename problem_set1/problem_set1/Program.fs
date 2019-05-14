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

    let foo = [1;2;3]
    let baz = [4;5;6]

    printfn "6). ANSWER: B"
    printfn " List.map List.head foo @ baz = %A \n value: list -> list -> list" (List.map List.head foo @ baz)
    printfn " ((List.map Listhead) foo) @ baz) = value: list -> list -> list\n"


problem6()




Console.ReadKey() |> ignore
