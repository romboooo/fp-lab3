module Interpolation

open Types

let generateXValues (xStart: float) (xEnd: float) (step: float) =
    if step <= 0.0 then failwith "Step must be positive"
    
    let rec generate acc currentX =
        if currentX <= xEnd + 1e-10 then
            generate (currentX :: acc) (currentX + step)
        else
            List.rev acc
    
    generate [] xStart

let linearInterpolation (points: Point list) (x: float) =
    if List.length points < 2 then
        failwith "At least two points are required for linear interpolation"
    let sorted = points |> List.sortBy (fun p -> p.X)

    let rec findSegment prev = function
        | [] -> None
        | curr::_ when x >= prev.X && x <= curr.X -> Some (prev, curr)
        | curr::rest -> findSegment curr rest

    match sorted with 
    | first::rest ->
        match findSegment first rest with
        | Some (p1, p2) ->
            if abs (p2.X - p1.X) < 1e-10 then
                p1.Y
            else
                let t = (x - p1.X) / (p2.X - p1.X)  // Исправлено: закрывающая скобка
                p1.Y * (1.0 - t) + p2.Y * t  // Исправлено: убрана лишняя скобка
        | None ->
            if x < first.X then    
                first.Y
            else
                (List.last sorted).Y
    | [] -> failwith "No points available for interpolation"

let newtonInterpolation (n: int) (points: Point list) (x: float) =
    if List.length points < n then
        failwithf "At least %d points are required for Newton interpolation" n

    let relevantPoints = 
        points 
        |> List.sortBy (fun p -> abs (p.X - x)) 
        |> List.take n

    let rec computeDividedDifferences (pts: Point list) (acc: float list) =
       match pts with
       | [] -> List.rev acc
       | [_] -> List.rev acc
       | _ ->
            let differences = 
                pts
                |> List.pairwise
                |> List.map (fun (p1, p2) -> (p2.Y - p1.Y) / (p2.X - p1.X))

            let newPts = 
                List.zip pts differences
                |> List.map (fun (p,d) -> {p with Y = d})
                |> List.tail

            computeDividedDifferences newPts (List.head differences :: acc)

    let xs = relevantPoints |> List.map (fun p -> p.X)
    let ys = relevantPoints |> List.map (fun p -> p.Y)

    let coefficients = 
        let first = List.head ys
        first :: computeDividedDifferences relevantPoints []

    let rec evaluate (coeffs: float list) (xVals: float list) (x: float) acc product =
        match coeffs, xVals with
        | [], _ -> acc
        | c::crest, xi::xrest -> 
            let newProduct = product * (x - xi)
            let newAcc = acc + c * newProduct
            evaluate crest xrest x newAcc newProduct
        | c::rest, [] ->
            acc + c * product
    evaluate coefficients.Tail xs.Tail x coefficients.Head 1.0

let getInterpolationFunction = function
    | Linear -> linearInterpolation
    | Newton n -> fun points x -> newtonInterpolation n points x

let getMethodName = function
    | Linear -> "linear"
    | Newton n -> sprintf "newton-%d" n