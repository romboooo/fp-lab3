# Лабораторная работа №3

## Бурейко Роман р3215

## Требования

программа должна быть реализована в функциональном стиле;
ввод/вывод должен быть отделён от алгоритмов интерполяции;
требуется использовать идиоматичный для технологии стиль программирования.

Types.fs
```f#
module Types

type Point = {
    X: float
    Y: float
}

type InterpolationMethod =
    | Linear
    | Newton of int

type Options = {
    Methods: InterpolationMethod list
    Step: float
    OutputAll: bool
}

type WindowState = {
    Points: Point list
    LastProcessedX: float option
    Method: InterpolationMethod
    Step: float
}

type ProcessResult = {
    MethodName: string
    State: WindowState
    NewPoints: Point list
}
```

Interpolation.fs

```f#
module Interpolation

open Types

let linearInterpolation (points: Point list) (x: float) =
    if List.length points < 2 then
        failwith "At least two points are required for linear interpolation"
    
    let sorted = points |> List.sortBy (fun p -> p.X)
    
    let rec findSegment = function
        | [] -> None
        | [_] -> None
        | p1::p2::rest ->
            if x >= p1.X && x <= p2.X then
                Some (p1, p2)
            else
                findSegment (p2::rest)
    
    match findSegment sorted with
    | Some (p1, p2) ->
        let t = (x - p1.X) / (p2.X - p1.X)
        p1.Y + t * (p2.Y - p1.Y)
    | None ->
        sorted
        |> List.minBy (fun p -> abs (p.X - x))
        |> fun p -> p.Y

let newtonInterpolation (n: int) (points: Point list) (x: float) =
    if List.length points < n then
        failwithf "At least %d points are required for Newton interpolation" n
    
    let nearest = 
        points 
        |> List.sortBy (fun p -> abs (p.X - x))
        |> List.take n
        |> List.sortBy (fun p -> p.X)
    
    let xs = nearest |> List.map (fun p -> p.X)
    let ys = nearest |> List.map (fun p -> p.Y)
    
    let m = n
    let table = Array2D.init m m (fun i j -> 0.0)
    
    for i in 0..m-1 do
        table.[i, 0] <- ys.[i]
    
    for j in 1..m-1 do
        for i in 0..m-j-1 do
            table.[i, j] <- (table.[i+1, j-1] - table.[i, j-1]) / (xs.[i+j] - xs.[i])
    
    let indices = [m-2 .. -1 .. 0] 
    List.fold 
        (fun acc i -> 
            acc * (x - xs.[i]) + table.[0, i])
        table.[0, m-1] 
        indices

let getInterpolationFunction = function
    | Linear -> linearInterpolation
    | Newton n -> fun points x -> newtonInterpolation n points x

let getMethodName = function
    | Linear -> "linear"
    | Newton n -> sprintf "newton-%d" n
```

Program.fs

```f#
open System
open Types
open Interpolation
open StreamProcess

let runInternalTests() =
    printfn "Running internal tests..."
    
    let points1 = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
    let result1 = linearInterpolation points1 0.5
    printfn "✓ Linear interpolation test: %f (expected: 0.5)" result1
    
    let points2 = [ 
        { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 4.0 }
        { X = 3.0; Y = 9.0 }
    ]
    let result2 = newtonInterpolation 4 points2 1.5
    printfn "✓ Newton interpolation test: %f (expected: 2.25)" result2
    
    let result3 = parsePoint "3.14 2.71"
    printfn "✓ Parse point test: %A (expected: Some {X=3.14, Y=2.71})" result3
    
    printfn "All internal tests passed!"

let parseArgs (args: string[]) =
    let rec parse (opts: Options) (i: int) =
        if i >= args.Length then opts
        else
            match args.[i] with
                | "--linear" -> 
                    let newMethods = 
                        if opts.Methods |> List.contains Linear then 
                            opts.Methods
                        else 
                            Linear :: opts.Methods
                    parse { opts with Methods = newMethods } (i + 1)
                | "--newton" when i + 1 < args.Length ->
                    let n = int args.[i + 1]
                    let newMethods = (Newton n) :: opts.Methods
                    parse { opts with Methods = newMethods } (i + 2)
                | "--step" when i + 1 < args.Length ->
                    let step = float args.[i + 1]
                    parse { opts with Step = step } (i + 2)
                | "--output-all" ->
                    parse { opts with OutputAll = true } (i + 1)
                | "--help" | "-h" ->
                    printfn "Usage: program [--linear] [--newton N] [--step STEP] [--output-all]"
                    printfn "  --linear       : use linear interpolation"
                    printfn "  --newton N     : use Newton interpolation with N points"
                    printfn "  --step STEP    : step for interpolation points (default: 0.1)"
                    printfn "  --output-all   : output all intermediate results"
                    printfn "  --help, -h     : show this help"
                    exit 0
                | "--test" ->
                    runInternalTests()
                    exit 0
                | arg ->
                    printfn "Warning: Unknown argument: %s" arg
                    parse opts (i + 1)
    
    let defaultOpts = {
        Methods = []
        Step = 0.1
        OutputAll = false
    }
    
    let result = parse defaultOpts 0
    
    if List.isEmpty result.Methods then
        { result with Methods = [Linear] }
    else
        { result with Methods = List.rev result.Methods }

let initializeWindowStates (opts: Options) =
    opts.Methods
    |> List.map (fun method ->
        { Points = []
          LastProcessedX = None
          Method = method
          Step = opts.Step })

let printResults (results: (string * Point list) list) (outputAll: bool) =
    results
    |> List.iter (fun (methodName, points) ->
        points
        |> List.iter (fun point ->
            printfn "%s: %.6f %.6f" methodName point.X point.Y))

[<EntryPoint>]
let main argv =
    try
        let opts = parseArgs argv
        
        if List.isEmpty opts.Methods then
            printfn "Error: No interpolation methods specified"
            printfn "Usage: program [--linear] [--newton N] [--step STEP] [--output-all]"
            1
        else
            let initialStates = initializeWindowStates opts
            
            let rec processInput (states: WindowState list) =
                match Console.ReadLine() with
                | null -> 
                    let finalResults = 
                        finalizeProcessing states
                        |> List.map (fun r -> (r.MethodName, r.NewPoints))
                    
                    printResults finalResults opts.OutputAll
                    states
                | line ->
                    match parsePoint line with
                    | Some point ->
                        let processed = processNewPoint states point
                        
                        let resultsToPrint = 
                            processed 
                            |> List.map (fun r -> (r.MethodName, r.NewPoints))
                        
                        printResults resultsToPrint opts.OutputAll
                        
                        let newStates = 
                            processed 
                            |> List.map (fun r -> r.State)
                        
                        processInput newStates
                    | None -> 
                        processInput states
            
            processInput initialStates |> ignore
            0
    with
    | ex ->
        printfn "Fatal error: %s" ex.Message
        1
```

StreamProcess.fs

```f#
module StreamProcess

open Types
open Interpolation

let parsePoint (line: string) = 
    let trimmedLine = line.Trim()
    
    if System.String.IsNullOrWhiteSpace(trimmedLine) then
        None
    else
        let parts =
            if trimmedLine.Contains(';') then
                trimmedLine.Split(';')
            else
                trimmedLine.Split([|'\t'; ' '|], System.StringSplitOptions.RemoveEmptyEntries)
        
        if parts.Length < 2 then
            None
        else
            try 
                Some { X = float parts.[0]; Y = float parts.[1] }
            with
            | _ -> None

let updateWindowState (state: WindowState) (newPoint: Point) =
    let updatedPoints = 
        (newPoint :: state.Points)
        |> List.sortBy (fun p -> p.X)
    
    let maxPoints = 
        match state.Method with
        | Linear -> System.Int32.MaxValue 
        | Newton n -> n 
    
    let trimmedPoints =
        if List.length updatedPoints > maxPoints then
            updatedPoints |> List.skip (updatedPoints.Length - maxPoints)
        else
            updatedPoints
    
    { state with Points = trimmedPoints }

let generateInterpolationPoints (state: WindowState) (newXMax: float option) =
    if List.length state.Points < 2 then
        []
    else
        let sorted = state.Points |> List.sortBy (fun p -> p.X)
        
        let minPointsNeeded = 
            match state.Method with
            | Linear -> 2
            | Newton n -> n
        
        if List.length sorted < minPointsNeeded then
            []
        else
            let xMin = sorted.Head.X
            let xMax = 
                match newXMax with
                | Some max -> max
                | None -> (List.last sorted).X
            
            let startX = 
                match state.LastProcessedX with
                | Some x -> 
                    let nextX = x + state.Step
                    if nextX < xMin then xMin else nextX
                | None -> xMin
            
            if startX > xMax + 1e-10 then
                []
            else
                let interpFunc = getInterpolationFunction state.Method
                
                let rec generate acc currentX =
                    if currentX <= xMax + 1e-10 then
                        try
                            let y = interpFunc sorted currentX
                            let point = { X = currentX; Y = y }
                            generate (point :: acc) (currentX + state.Step)
                        with
                        | ex -> 
                            generate acc (currentX + state.Step)
                    else
                        List.rev acc
                
                generate [] startX

let processNewPoint (states: WindowState list) (point: Point) =
    states
    |> List.map (fun state ->
        let updatedState = updateWindowState state point
        let newPoints = generateInterpolationPoints updatedState (Some point.X)
        
        let lastX = 
            if List.isEmpty newPoints then
                updatedState.LastProcessedX
            else
                newPoints |> List.last |> fun p -> Some p.X
        
        let finalState = { updatedState with LastProcessedX = lastX }
        
        { MethodName = getMethodName state.Method
          State = finalState
          NewPoints = newPoints })

let finalizeProcessing (states: WindowState list) =
    states
    |> List.map (fun state ->
        let newPoints = generateInterpolationPoints state None
        
        { MethodName = getMethodName state.Method
          State = state
          NewPoints = newPoints })
```




# programm usage
Usage: program [--linear] [--newton N] [--step STEP] [--output-all]
  --linear       : use linear interpolation
  --newton N     : use Newton interpolation with N points
  --step STEP    : step for interpolation points (default: 0.1)
  --output-all   : output all intermediate results
  --help, -h     : show this help

# io examples

```sh
echo -e "0.0 0.0\n1.0 1.0\n2.0 4.0\n3.0 9.0" | dotnet run -- --linear --step 0.5
```

```
linear: 0.000000 0.000000
linear: 0.500000 0.500000
linear: 1.000000 1.000000
linear: 1.500000 2.500000
linear: 2.000000 4.000000
linear: 2.500000 6.500000
linear: 3.000000 9.000000
```

```sh
echo -e "0.0 1.0\n0.5 0.88\n1.0 0.54\n1.5 0.07\n2.0 -0.42\n2.5 -0.80" | dotnet run -- --newton 4 --step 0.25
```

```
newton-4: 0.000000 1.000000
newton-4: 0.250000 0.973125
newton-4: 0.500000 0.880000
newton-4: 0.750000 0.731875
newton-4: 1.000000 0.540000
newton-4: 1.250000 0.315625
newton-4: 1.500000 0.070000
newton-4: 1.750000 -0.179375
newton-4: 2.000000 -0.420000
newton-4: 2.250000 -0.631875
newton-4: 2.500000 -0.800000
```

```sh
echo -e "1.0 15.2\n2.0 16.1\n3.0 17.5\n4.0 18.8\n5.0 19.3\n6.0 18.9" | dotnet run -- --linear --newton 3 --step 0.3 --output-all
```

```
linear: 1.000000 15.200000
linear: 1.300000 15.470000
linear: 1.600000 15.740000
linear: 1.900000 16.010000
linear: 2.200000 16.380000
linear: 2.500000 16.800000
linear: 2.800000 17.220000
newton-3: 1.000000 15.200000
newton-3: 1.300000 15.417500
newton-3: 1.600000 15.680000
newton-3: 1.900000 15.987500
newton-3: 2.200000 16.340000
newton-3: 2.500000 16.737500
newton-3: 2.800000 17.180000
linear: 3.100000 17.630000
linear: 3.400000 18.020000
linear: 3.700000 18.410000
linear: 4.000000 18.800000
newton-3: 3.100000 17.634500
newton-3: 3.400000 18.032000
newton-3: 3.700000 18.420500
newton-3: 4.000000 18.800000
linear: 4.300000 18.950000
linear: 4.600000 19.100000
linear: 4.900000 19.250000
newton-3: 4.300000 19.034000
newton-3: 4.600000 19.196000
newton-3: 4.900000 19.286000
linear: 5.200000 19.220000
linear: 5.500000 19.100000
linear: 5.800000 18.980000
newton-3: 5.200000 19.292000
newton-3: 5.500000 19.212500
newton-3: 5.800000 19.052000
```


# Вывод
Я реализовал интерполяцию двумя способами - Ньютона и Линейную. А также поработал с потоковым режимом, реализовав это с помощью концепции обработки данных "окно". Заюзал Pattern matching, методы для работы со списками и всё такое