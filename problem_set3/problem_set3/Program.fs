// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

//create discriminated union that can represent a simple linked list of ints
type linkedList<'a> = 
    | E
    | L of 'a * linkedList<'a>

let problem1()=
    //write a function that converts a list into a linked list of nodes

    let rec aList = function
    | E -> 0
    | L(x,xs) -> x + (aList xs)

    //I'm not sure if this is correct.

    

problem1()

let problem2()=
    (*
    This CFG recognizes some strings of zeros and ones.
        S → 0A | 1B
        A → 0AA | 1S | 1
        B → 1BB | 0S | 0 
Describe the strings that the CFG recognizes.
This language is ambiguous. Find a string that is recognized by this grammar which has two derivations.
Show the two derivation trees for the string in part (b).
    
    *)

    //

problem2()




Console.ReadKey() |> ignore
