open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

//Exercise 1.1
let sqr x = x * x

//Exercise 1.2
let pow (x:float) (n:float) :float =
    Math.Pow (x,n)

//Exercise 1.3
let rec sum x =
    if x > 0 then 
        x + sum (x - 1) 
    else 
        0

//Exercise 1.4
let rec fib (n : int) =
    if n = 0 then
        0
    else if n = 1 then
        1
    else
        fib (n - 1) + fib (n - 2)


//Exercise 1.5
let dup (s :string) =
    s + s

//Exercise 1.6
let rec dupn (s:string) (n:int) :string =
    if n > 0 then
        s + dupn s (n - 1)
    else
        ""

//Exercise 1.7
let rec bin ((n,k):int * int) : int =
    if k = 0 || n = k then
        1
    else
        bin ((n - 1), (k - 1)) + bin ((n - 1), k)

[<EntryPoint>]
let main argv =
    printfn "1.1 expected %d actual %d" 4 (sqr 2)
    printfn "1.2 expected %f actual %f" 8.0 (pow 2.0 3.0)
    printfn "1.3 expected %d actual %d" 15 (sum 5)
    printfn "1.4 expected %d actual %d" 3 (fib 4)
    printfn "1.5 expected %s actual %s" "Hi Hi " (dup "Hi ")
    printfn "1.6 expected %s actual %s" "Hi Hi Hi " (dupn "Hi " 3)
    printfn "1.7 expected %d actual %d" 6 (bin (4, 2))
    
    0 // return an integer exit code