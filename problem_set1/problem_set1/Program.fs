open System
open System.Runtime.InteropServices
open System.Diagnostics

//problem set 1

let problem1()=
    printfn "1) ANSWER: C"
    printfn "  a). 2 + 5 * 10 = %d" (2 + 5 * 10)
    printfn "  b). 10I * 20I = %A" (10I + 20I)
    printfn "  c). 4 + 5.6 - cannot add float and int because they are different types."
    printfn "  d). Strings can be added = %s\n" ("4" + "5.6")

problem1()



Console.ReadKey() |> ignore
