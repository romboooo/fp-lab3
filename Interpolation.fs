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