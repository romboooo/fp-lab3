open System
open Types
open Interpolation
open StreamProcess

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
                    // EOF reached
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
                        // Skip invalid lines
                        processInput states
            
            processInput initialStates |> ignore
            0
    with
    | ex ->
        printfn "Fatal error: %s" ex.Message
        1