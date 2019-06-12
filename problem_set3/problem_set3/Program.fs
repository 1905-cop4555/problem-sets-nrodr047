
open System

//create discriminated union that can represent a simple linked list of ints
type linkedList<'a> = 
    | E
    | L of 'a * linkedList<'a>
 (*
let problem1()=
    //write a function that converts a list into a linked list of nodes

    let rec aList = function
    | E -> 0
    | L(x,xs) -> x + (aList xs)



    //I'm not sure if this is correct.

    

problem1()

*)

(*
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

    // If string starts with 0 then, 0AA or eat 1. If string starts with 1 then 1BB or eat 0. 
  

problem2()
*)

(*
let problem3()=

   

    Write a CFG to recognize palindromes over the alphabet {a, b, |}, with the bar in the middle.
Write a parse function that accepts a string and generates tokens for the language.
Write a syntax checker that verifies if a list of tokens represents a palindrome.
Extend the syntax checker so it generates an abstract syntax tree and displays it, for valid palindromes.
    
    

    // S -> aSa | bSb | a | b | i

problem3()
*)

(*
let problem4()=

    
    
    Using the natural semantics from the lecture notes, show all the steps for verifying each judgement. 
[When writing derivations, I tend to set a variable to the current state of memory, to save typing. My derivations are six lines, six lines, and 22 lines.]


        ************************RULES FROM LECTURE NOTES****************************************

            (M,n) => M                          *an integer literal n evaluates to n, regardless of memory

            hypothesis
            ----------
            conclusion

            (M, skip) => M                      *rule for skip

            x is in dom(M)  (M,e)=>n
            ------------------------             *notation M[x:=n] denotes a new memory where x is in n
            (M, x:=e) => M[x:=n]                 * all other variables have same value in M
                                                    *EXAMPLE:
                                                        {x=5, y=2} [x:=7] = {x=7, y=2}  *new memory hold new value*
            
            (M, c1) => M'  (M',c2) => M''
            -----------------------------        *Rule of sequential
            (M, c1;c2) => M''                       *EXAMPLE:
                                                        ({i=7, j=5}, i:=i+1, i:=2*i+j) => {i=21, j=5}
                                                                    7 + 1 = 8
                                                                    2 * 8 + 5 = 21

            
            (M,e) => n  n nonzero (M,c1) => M'
            ----------------------------------     *Rule 1 of if then else
            (M, if e then c1 else c2) => M'

            (M,e) => 0 (M,c2) => M' 
            -----------------------                 *Rule 2 of if then else
            (M, if e then c1 else c2) => M'


            (M,e) => 0
            ----------------                       *Rule for while e do c (SKIP)
            (M, while e do c) => M


            (M,e) => n nonzero (M,c)=> M' (M', while e do c) => M''
            ------------------------------------------------------           *nonzero, change memory, while loop, change memory again.
            (M, while e do c ) => M''

      *******************************************************************************************************

    
    
    ({i=5; j=8}, i := 2*j + i) => {i=21; j=8}
       i is in the dom({i=5;j=8})               //first statement is an assignment
       ({i=5;j=8}), i => 5                      // + operator simple rule, evaluate i  EQ1
       ({i=5;j=8}), 2 => 2                      // constant is always a constant EQ2
       ({i=5; j=8}), j => 8                     //j = 8, EQ1 & EQ2
       ({i=5; j=8}), 2 * j => 2 * 8 = 16
       ({i=5; j=8}), 16 + i => 16 + 5 = 21

        
   ({i=3; j=8}, if (2*i > j) then i := 2*j else j := 2*i) => {i=3; j=6}
        ({i=3; j=8}, if (2 * i > j) then i is the the dom({i=5;j=8}) else j is in the dom(i=5; j=8})
        ({i=3;j=8}), i => 3
        ({i=3;j=8}), 2 => 2
        ({i=3;j=8}), 2 * 3 => 6
        ({i=3;j=8}), j => 8
        ({i=3; j=8}), 6 > 8 => j = 2 * i
        ({i=3; j=8}), j = 2 * 3 => 6


    
    ({i=1; j=10}, while (3*i <= j) do i := 3*i) => {i=9; j=10}
    
    




problem4()
*)


let problem5()=

    let rec interleave x y acc =
        match (x,y) with
        | ([],[]) -> acc
        | ([],_) -> failwith "Different lengths"
        | (_,[]) -> failwith "Different lengths"
        | (x::xs, y::ys) -> x::y::interleave xs ys (acc)

    let x = [1;2;3]
    let y = [4;5;6]

    let inter = interleave x y []

    printfn("Problem 5: interleave x y = %A") inter

    //tail recursion is faster

problem5()

type 'a stream = Cons of 'a * (unit -> 'a stream)
//infinite stream

let problem7()=
    (*Multiples of a list
Generate an infinite stream for the the natural numbers greater than zero 
that are divisible by each element in a list of four elements. 
Use four, nested calls of filter on the infinite stream of natural numbers starting at one. 
For example the output for the list [2;3;21;10]:
210, 420, 630, 840, 1050, ...
Display the 20th through 30th numbers in the series.
Repeat the exercise using an infinite sequence. 
Sequences also have a filter function, so it can be solved similarly to the infinite stream version. 
Just for fun, try to solve it without using the filter function.
For both functions, be sure to dislay an appropriate error message if the list does not have exactly four elements.*)


    let rec upfrom n = Cons(n, fun()->upfrom(n+1)) //natural numbers in an infinite list
    
    let nats = upfrom 1 //natural numbers greater than 0

    let rec take n (Cons(x,xsf))=
        match n with
        | 0 -> []
        | n -> x::take(n-1)(xsf())

    let rec filter p (Cons(x,xsf)) =                    //filter function
        if p x then Cons(x, fun()->filter p (xsf()))  
        else filter p (xsf())

   

    //divisible function
    let rec div (Cons(x,xsf)) =
        Cons(x, fun() -> div (filter (fun n -> n%x = 0) (xsf())))
 

    let result = div (upfrom 1) 
    printfn "Problem 7: %A" result

    let rec eratosthenes (Cons(x, xsf)) =
      Cons(x, fun () -> eratosthenes (filter (fun n -> n%x <> 0) (xsf())))

    let primes = eratosthenes (upfrom 2)

    printfn "\n Eratosthene: %A" primes


problem7()







Console.ReadKey() |> ignore
