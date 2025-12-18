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
                    // Run internal tests and exit
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