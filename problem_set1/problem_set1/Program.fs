open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Linq.Expressions
open System.Linq.Expressions
open System.Runtime.InteropServices
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.InteropServices

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

let problem3()=

     printfn "3). ANSWER: A\n    (float -> float) -> bool\n     is not a legal type\n"
            // let notLegal = (4.5 + 3.2) = true 
problem3()

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
    printfn "   d). fun x -> x::[5] value: int -> int list\n" // int x appends to an int list

problem10()

let problem11()=

    printfn "11). ANSWER: A"
    printfn "  (3, [], true) is the threeple. 3 defaults to int. [] defaults to a 'a list. true is a boolean"
    printfn "  Therefore, the value: int * 'a list * bool\n"

problem11()

let problem12()=

    printfn "12). ANSWER: A"
    printfn "  a). fun x y -> x+y+\".\" value: string -> string -> string as expected from a curried function\n"


problem12()
  

let problem13()=

    printfn "13). ANSWER: D"
    printfn "   fun xs -> List.map (+) xs values to int list -> (int -> int) list"
    printfn "   The (+) operator values to (int -> int) in which List.map acts upon xs to make the function into (int -> int) list"
    printfn "   Therefore fun xs is an int list\n"


problem13()

let problem14()=
    
    printfn "14). ANSWER: NONE"
    printfn "  a). fun x -> fun y -> x y \".\" values to 'a -> string -> 'b"
    printfn "  b). fun x y -> String.length x * String.length y values to string -> string -> int" // the * operator is multiplying
    printfn "  c). fun(x,y) -> x + y + \".\" values to (string * string) -> string which is a tuple that returns a string"
    printfn "  d). (+) values to int -> int -> int and it is missing members\n"
 

problem14()


let problem15()=
    
    printfn "15). ANSWER: C"
    printfn "  a). fun f -> String.length (f \"cat\") values to (string -> string) -> int"
    printfn "  b). fun x y -> x \" \" + y values to string -> string -> string"
    printfn "  c). fun f -> f (f \"cat\") values to (string -> string) -> string"
    printfn "  d). fun f -> f \"cat\" values to (string -> 'a) -> 'a\n"

problem15()

let problem16()=

    //auxiliary function
    let rec gcd = function
    |(a, 0) -> a //if tuple is (a, 0) return a
    |(a,b) -> gcd(b, a%b) //else call gcd with tuple (b, a%b)

    let (.+) (a,b) (c,d) =
        let top = (a * d) + (b * c) //cross multiply and add
        let bottom = (b * d) //mutilpy bottom denominators
        (top/gcd(top,bottom), bottom/gcd(top,bottom)) //get gcd 

    let (.*) (a,b) (c,d) =
        let top = (a * c) //multiply top
        let bottom = (b * d) //multiply bottom
        (top/gcd(top,bottom), bottom/gcd(top,bottom)) //get gcd 

    printfn "16) (1,2) (.+) (1,3) = %A" ((.+) (1,2) (1,3))
    printfn "    (1,2) (.+) (1,3) (.*) (3,7) = %A\n" ((.*) (3,7) ((.+) (1,2) (1,3))) //why is it not 11? FIX ME


problem16()

let problem17()=

    let a = [[0;1;1];[3;2];[];[5]]

    let revlist xs = List.map (fun x -> List.rev x) xs
    //List.map is like a for loop
    //for every x in xs, List.rev reverses their order

    printfn "17). a = %A" (a)
    printfn "   revlist a = %A\n" (revlist a)

problem17()


let problem18()=

    //problem 18 - 21


    //PROBLEM 18***************************************
    let xs = [1;2;3]
    let ys = [4;5;6]

    //takes a tuple
    let rec interleave = function
        | ([],ys) -> ys //if xs is empty return ys
        | (xs,[]) -> xs //if ys is empty return xs
        | (x::xs, y::ys) -> x::y::interleave(xs,ys) //take heads from list and call interleave for tail
    
    printfn "18). xs = %A"(xs)
    printfn "     ys = %A"(ys)
    printfn "     interleave(xs,ys) = %A\n" (interleave(xs,ys))


    //PROBLEM 19***************************************

    (* let xs = [1;2;3;4;5;6]

    let rec gencut(n,xs) = function
        | n, [] -> xs
        | 0, xs -> xs
        | n, x::xs -> let small = x::gencut(n-1, xs)


    let cut xs =
        let n = (List.length xs) / 2
        gencut(n,xs)

    printfn "19). xs = %A"(xs)
    printfn "     cut xs = %A" (cut xs)

    //PROBLEM 20 **************************************** *)



    
problem18()

let problem22()=

    let xs = ["a";"b";"c"]
    let ys = [1;2]

    let cartesian(xs,ys) = List.map(fun x -> List.map(fun y -> (x,y))ys)xs

    //Notes:
    // Maps through the x in list xs
    // Maps x to y in list ys
    // and makes a tuple - cartesian pair

    printfn "22). cartesian(xs,ys) = %A\n" (cartesian(xs,ys))

problem22()

let problem23()=

        let xs = [1;2;3]

        let rec powerset = function
            | [] -> [[]] //returns the needed empty set to the list
            | x::xs -> let ys = powerset xs //creates a new list ys and calls powerset xs
                       List.map(fun xs -> x::xs) ys @ ys //scrolls through xs and appends x to ys 


        //visually ---
        // xs = 1;2;3
        // x::xs = (1::2;3)-> ys = powerset xs = 2;3
        // List.map x::xs = 1::2;3
        // x @ ys = [1;2;3]
        // x::y = 1::2 = [1;2] @ ys = [[1;2;3];[1;2]]
        // x::y = 1::3 = [1;3] @ ys = [[1;2;3];[1;2];[1;3]]
        // x::y = 1::[] = [1] @ ys = [[1;2;3];[1;2];[1]]


        printfn "23). powerset xs = %A\n" (powerset xs)

                



problem23()

let problem24()=

    let xs = [[1;2;3];[4;5;6]]
    
    let rec transpose = function
        | [[];_] -> []
        | xs -> List.map List.head xs::transpose(List.map List.tail xs)


    printfn "24). transpose(xs,ys) = %A\n"(transpose xs)
    
problem24()


let problem25()=

    (*
   
   let rec sort = function
	    | []         -> []
	    | [x]        -> [x]
	    | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
            else x2 :: sort (x1::xs)
    *)

    //1. All basecases are correct - Yes, they are.
    //2. Assume the recursion call works - The recursion call does not sort correctly
            (*
                
                    1 :: sort [3;4;1;5;9;2;6;5]
                    3 :: sort [4;1;5;9;2;6;5]
                    1 :: sort [4;5;9;2;6;5]
                    4 :: sort [5;9;2;6;5]
                    5 :: sort [9;2;6;5]
                    2 :: sort [9;6;5]
                    6 :: sort [9;5]]
                    5 :: sort [9]
                    9 :: Base case\n
                
            *)
    //3. The recursion call is smaller than the input - Yes, they are smaller.
    //4. All basecases are covered - Yes.

    printfn "25). Answer is commented out in the code...\n"

problem25()

let problem26()=

    (*
    
    let rec merge = function
	    | ([], ys)       -> ys
	    | (xs, [])       -> xs
	    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
            else y :: merge (x::xs, ys)

	    let rec split = function
	    | []       -> ([], [])
	    | [a]      -> ([a], [])
	    | a::b::cs -> let (M,N) = split cs
            (a::M, b::N)

	    let rec mergesort = function
	    | []  -> []
	    | L   -> let (M, N) = split L
            merge (mergesort M, mergesort N)
    
    *)

    printfn "26). F# infers that mergesort is a ('a list -> b' list) creating a whole separate type list
    when it should be 'a list -> a list while sorting the same list.
    I believe that the recursive call to mergesort is incorrect (step 3). I think that the
    call would make a type of infinite loop. I think mergesort( merge M, merge N) would work."



problem26()


let problem27()=


   printfn "27). Context free grammars"
   printfn "E -> E+T | E-T | T"
   printfn "T -> T*F | T/F | F"
   printfn "F -> i | (E)"
   printfn "G -> G ^ F | G //highest precedence, right associative.\n" //left associative = G -> F ^ G | G
    
problem27()

let problem28()=

    printfn "28). S -> if (E) S
                    -> if (E) S else S
                    -> begin S L 
                    -> print E
                    
     AMBIGOUS:        if
                    E    if
                       E  S  S
           if 
         E    S
           if
          E  S"

problem28()

Console.ReadKey() |> ignore
