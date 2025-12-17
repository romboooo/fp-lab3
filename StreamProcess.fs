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
            failwith (sprintf "Invalid point format: %s" line)
        else
            try 
                Some { X = float parts.[0]; Y = float parts.[1] }
            with
            | _ -> failwith (sprintf "Cannot parse numbers: %s" line)

let updateWindowState (state: WindowState) (newPoint: Point) =
    let updatedPoints = 
        (newPoint :: state.Points)
        |> List.sortBy (fun p -> p.X)

    let maxPoints = 
        match state.Method with
        | Linear -> 2
        | Newton n -> n
    
    let trimmedPoints =
        if List.length updatedPoints > maxPoints then
            updatedPoints |> List.rev |> List.take maxPoints |> List.rev
        else
            updatedPoints
    { state with Points = trimmedPoints }

let generateInterpolationPoints (state: WindowState) (newXMax: float option) =
    if List.length state.Points < 2 then
        []  
    else
        let sorted = state.Points |> List.sortBy (fun p -> p.X)
        
        // Для линейной интерполяции берём последние 2 точки
        let pointsForInterpolation = 
            match state.Method with
            | Linear -> sorted |> List.rev |> List.take 2 |> List.rev
            | Newton n -> sorted |> List.rev |> List.take n |> List.rev
        
        let xMin = pointsForInterpolation.Head.X
        let xMax = 
            match newXMax with
            | Some max -> max
            | None -> pointsForInterpolation |> List.last |> fun p -> p.X
        
        let startX = 
            match state.LastProcessedX with
            | Some x -> x + state.Step
            | None -> xMin
        
        if startX > xMax + 1e-10 then
            []
        else
            let interpFunc = getInterpolationFunction state.Method
            
            let rec generate acc current =
                if current <= xMax + 1e-10 then
                    generate (current :: acc) (current + state.Step)
                else
                    List.rev acc
            
            let xValues = generate [] startX
            
            let filteredXValues = 
                xValues 
                |> List.filter (fun x -> x <= xMax + 1e-10)
            
            filteredXValues
            |> List.map (fun x -> 
                try
                    let y = interpFunc pointsForInterpolation x
                    Some { X = x; Y = y }
                with
                | _ -> None)
            |> List.choose id

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