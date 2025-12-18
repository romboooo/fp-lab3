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
        | Linear -> System.Int32.MaxValue // Keep all points for linear
        | Newton n -> n // Keep only n points for Newton
    
    let trimmedPoints =
        if List.length updatedPoints > maxPoints then
            // For Newton: keep only the n most recent points (by X, assuming sorted input)
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
                            // Skip this point
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