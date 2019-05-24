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

    let plus = curry (+)
    plus (2,3)




problem1()

Console.ReadKey() |> ignore