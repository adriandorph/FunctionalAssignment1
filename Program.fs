open System

//Green
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

//Yellow
//Exercise 1.8
let timediff ((h1,m1):int * int) ((h2,m2):int * int) : int =
    let HDiff = (h2 - h1) * 60
    let MDiff = m2 - m1
    let total = HDiff + MDiff
    if total > 60 * 12 then
        -(24 * 60 - total)
    else if total < 60 * -12 then
        24 * 60 + total
    else
        total

let timediffRaw ((h1,m1):int * int) ((h2,m2):int * int) : int =
    let HDiff = (h2 - h1) * 60
    let MDiff = m2 - m1
    HDiff + MDiff

//Exercise 1.9
let minutes ((hours, minutes): int * int) : int =
    timediffRaw (0,0) (hours, minutes)

let curry (functio:int * int -> int) : int -> int -> int=
    (fun x y -> functio (x, y))

let uncurry (functio: int -> int -> int) : int * int -> int =
    (fun (x,y) -> functio x y)


//Testing
let assertTrue (statement:bool) : string =
    if statement then
        "PASSED"
    else 
        "FAILED Expected: True Actual: False"

let assertEqualInt (expected:int) (actual:int) : string =
    if expected = actual then
        "PASSED"
    else
        sprintf "FAILED Expected: %d Actual: %d" expected actual

let assertEqualFloat (expected:float) (actual:float) : string =
    if expected = actual then
        "PASSED"
    else
        sprintf "FAILED Expected: %f Actual: %f" expected actual

let assertEqualString (expected:string) (actual:string) : string =
    if expected = actual then
        "PASSED"
    else
        sprintf "FAILED Expected: %s Actual: %s" expected actual



//Tests
//Green
let testSqr =
    assertEqualInt 4 (sqr 2)

let testPow =
    assertEqualFloat 8.0 (pow 2.0 3.0)
    
let testSum =
    assertEqualInt 15 (sum 5)

let testFib =
    assertEqualInt 3 (fib 4)

let testDup = 
    assertEqualString "Hi Hi " (dup "Hi ")

let testDupn =
    assertEqualString "Hi Hi Hi " (dupn "Hi " 3)

let testBin =
    assertEqualInt 6 (bin (4, 2))

//Yellow
let testTimediff =
    assertEqualInt -10 (timediff (0,5) (23,55))

let testTimediffRaw1 =
    assertEqualInt -59 (timediffRaw (12,34) (11,35))

let testTimediffRaw2 =
    assertEqualInt 61 (timediffRaw (12,34) (13,35))

let testMinutes =
    assertEqualInt 864 (minutes (14, 24))

let testCurry =
    assertEqualInt 8 (curry (fun (x, y) -> (x + y)) 5 3)

let testUncurry =
    assertEqualInt 8 (uncurry (fun x y -> x + y) (5, 3))

[<EntryPoint>]
let main argv =
    printfn "GREEN Exercises:"
    printfn "1.1 %s" testSqr
    printfn "1.2 %s" testPow
    printfn "1.3 %s" testSum
    printfn "1.4 %s" testFib
    printfn "1.5 %s" testDup
    printfn "1.6 %s" testDupn
    printfn "1.7 %s" testBin

    printfn "\nYELLOW Exercises"
    printfn "1.8 %s" testTimediff
    printfn "1.8 raw %s" testTimediffRaw1
    printfn "1.8 raw %s" testTimediffRaw2
    printfn "1.9 %s" testMinutes
    printfn "1.10 curry %s" testCurry
    printfn "1.10 uncurry %s" testUncurry
    0 // return an integer exit code
