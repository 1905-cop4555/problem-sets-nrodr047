// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System



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

Console.ReadKey() |> ignore