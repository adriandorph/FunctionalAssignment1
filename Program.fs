open System
//Some parts of the code have been copied from the assignment document examples

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
let timediffReal ((h1,m1):int * int) ((h2,m2):int * int) : int =
    let HDiff = (h2 - h1) * 60
    let MDiff = m2 - m1
    let total = HDiff + MDiff
    if total > 60 * 12 then
        -(24 * 60 - total)
    else if total < 60 * -12 then
        24 * 60 + total
    else
        total

let timediff ((h1,m1):int * int) ((h2,m2):int * int) : int =
    let HDiff = (h2 - h1) * 60
    let MDiff = m2 - m1
    HDiff + MDiff

//Exercise 1.9
let minutes ((hours, minutes): int * int) : int =
    timediff (0,0) (hours, minutes)

//Exercise 1.10
let curry functio =
    (fun x y -> functio (x, y))

let uncurry functio =
    (fun (x,y) -> functio x y)

//Exercise 1.11
let empty pair : int -> char * int =
    (fun x -> pair)

//RED
//Exercise 1.12
let add newPosition ((character, integer):char * int) (word:(int -> char * int)) = 
    fun pos -> (
        if pos = newPosition then
            (character, integer)
        else
            word pos
        )

//Exercise 1.13
let hello =
    add 4 ('O', 1) (add 3 ('L', 1) (add 2 ('L', 1) (add 1 ('E', 1) (add 0 ('H', 4) (empty (char 0, 0))))))


//Exercise 1.14
let singleLetterScore hello pos =
    let character, point = hello pos
    point

let doubleLetterScore hello pos =
    (singleLetterScore hello pos) * 2

let trippleLetterScore hello pos =
    (singleLetterScore hello pos) * 3




//The rest is only for testing
//I know there is an easier way to test, but I wanted to do it like this for fun

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

let assertEqualChar (expected:char) (actual:char) : string =
    if expected = actual then
        "PASSED"
    else
        sprintf "FAILED Expected: %c Actual: %c" expected actual



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
let testTimediffReal =
    assertEqualInt -10 (timediffReal (0,5) (23,55))

let testTimediff1 =
    assertEqualInt -59 (timediff (12,34) (11,35))

let testTimediff2 =
    assertEqualInt 61 (timediff (12,34) (13,35))

let testMinutes =
    assertEqualInt 864 (minutes (14, 24))

let testCurry =
    assertEqualInt 8 (curry (fun (x, y) -> (x + y)) 5 3)

let testUncurry =
    assertEqualInt 8 (uncurry (fun x y -> x + y) (5, 3))

let testEmpty =
    //Arrange
    let funct = empty ('a',3)
    //Act
    let charater, integer = funct 4
    //Assert
    assertEqualInt 3 integer + " " + assertEqualChar 'a' charater

//RED
let testAdd =
    //Arrange
    let theLetterA = empty ('A', 1)
    let theLettersAB = add 1 ('B', 3) theLetterA

    //Act
    let character0, integer0 = theLettersAB 0
    let character1, integer1 = theLettersAB 1
    let character42, integer42 = theLettersAB 42
    //Assert
    sprintf "%s %s\n%s %s\n%s %s" (assertEqualChar 'A' character0) (assertEqualInt 1 integer0) (assertEqualChar 'B' character1) (assertEqualInt 3 integer1) (assertEqualChar 'A' character42) (assertEqualInt 1 integer42)

let testHello =
    //Arrange
    let helloWord pos :char * int=
        hello pos

    //Act
    let character0, integer0 = helloWord 0
    let character1, integer1 = helloWord 1
    let character2, integer2 = helloWord 2
    let character3, integer3 = helloWord 3
    let character4, integer4 = helloWord 4
    //Assert
    sprintf "%s %s\n%s %s\n%s %s\n%s %s\n%s %s" (assertEqualChar 'H' character0) (assertEqualInt 4 integer0) (assertEqualChar 'E' character1) (assertEqualInt 1 integer1) (assertEqualChar 'L' character2) (assertEqualInt 1 integer2) (assertEqualChar 'L' character3) (assertEqualInt 1 integer3) (assertEqualChar 'O' character4) (assertEqualInt 1 integer4)

let testLetterPoint =
    //Arrange
    let helloWord pos :char * int =
        hello pos
    
    //Act
    let actual1 = singleLetterScore helloWord 0
    let actual2 = doubleLetterScore helloWord 0
    let actual3 = trippleLetterScore helloWord 0

    sprintf "%s %s %s" (assertEqualInt 4 actual1) (assertEqualInt 8 actual2) (assertEqualInt 12 actual3)


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
    printfn "1.8 %s" testTimediffReal
    printfn "1.8 raw %s" testTimediff1
    printfn "1.8 raw %s" testTimediff2
    printfn "1.9 %s" testMinutes
    printfn "1.10 curry %s" testCurry
    printfn "1.10 uncurry %s" testUncurry
    printfn "1.11 %s" testEmpty

    printfn "\nRED Exercises"
    printfn "1.12 %s" testAdd
    printfn "1.13 %s" testHello
    printfn "1.14 %s" testLetterPoint
    0 // return an integer exit code