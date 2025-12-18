module Tests

open System
open Xunit
open Types
open Interpolation
open StreamProcess

[<Fact>]
let ``parsePoint with space separator`` () =
    let result = parsePoint "1.0 2.0"
    Assert.Equal(Some { X = 1.0; Y = 2.0 }, result)

[<Fact>]
let ``parsePoint with semicolon separator`` () =
    let result = parsePoint "1.0;2.0"
    Assert.Equal(Some { X = 1.0; Y = 2.0 }, result)

[<Fact>]
let ``parsePoint with tab separator`` () =
    let result = parsePoint "1.0\t2.0"
    Assert.Equal(Some { X = 1.0; Y = 2.0 }, result)

[<Fact>]
let ``parsePoint invalid string returns None`` () =
    let result = parsePoint "invalid"
    Assert.Equal(None, result)

[<Fact>]
let ``parsePoint empty string returns None`` () =
    let result = parsePoint ""
    Assert.Equal(None, result)

[<Fact>]
let ``parsePoint with extra spaces`` () =
    let result = parsePoint "  1.5   3.7  "
    Assert.Equal(Some { X = 1.5; Y = 3.7 }, result)

[<Fact>]
let ``linearInterpolation between two points`` () =
    let points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
    let result = linearInterpolation points 0.5
    Assert.Equal(0.5, result, 5)

[<Fact>]
let ``linearInterpolation exact point`` () =
    let points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 }; { X = 2.0; Y = 4.0 } ]
    let result = linearInterpolation points 1.0
    Assert.Equal(1.0, result, 5)

[<Fact>]
let ``linearInterpolation extrapolation left`` () =
    let points = [ { X = 1.0; Y = 1.0 }; { X = 2.0; Y = 4.0 } ]
    let result = linearInterpolation points 0.5
    Assert.Equal(1.0, result, 5)

[<Fact>]
let ``linearInterpolation extrapolation right`` () =
    let points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
    let result = linearInterpolation points 1.5
    Assert.Equal(1.0, result, 5)

[<Fact>]
let ``linearInterpolation with unsorted points`` () =
    let points = [ { X = 2.0; Y = 4.0 }; { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
    let result = linearInterpolation points 0.5
    Assert.Equal(0.5, result, 5)

[<Fact>]
let ``newtonInterpolation for quadratic function with 3 points`` () =
    let points = [ 
        { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 4.0 }
    ]
    let result = newtonInterpolation 3 points 1.5
    Assert.Equal(2.25, result, 5)

[<Fact>]
let ``newtonInterpolation for linear function with 2 points`` () =
    let points = [ 
        { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
    ]
    let result = newtonInterpolation 2 points 0.5
    Assert.Equal(0.5, result, 5)

[<Fact>]
let ``newtonInterpolation exact point match`` () =
    let points = [ 
        { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 4.0 }
        { X = 3.0; Y = 9.0 }
    ]
    let result = newtonInterpolation 4 points 2.0
    Assert.Equal(4.0, result, 5)

[<Fact>]
let ``updateWindowState for Linear method`` () =
    let state = { 
        Points = []
        LastProcessedX = None
        Method = Linear
        Step = 0.1
    }
    let point = { X = 1.0; Y = 2.0 }
    let newState = updateWindowState state point
    let expectedPoints = [point]
    Assert.Equal<Point list>(expectedPoints, newState.Points)
    Assert.Equal(None, newState.LastProcessedX)

[<Fact>]
let ``updateWindowState for Newton method keeps only n points`` () =
    let state = { 
        Points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
        LastProcessedX = None
        Method = Newton 2
        Step = 0.1
    }
    let point = { X = 2.0; Y = 4.0 }
    let newState = updateWindowState state point
    Assert.Equal(2, newState.Points.Length)
    let sortedPoints = newState.Points |> List.sortBy (fun p -> p.X)
    Assert.Equal({ X = 1.0; Y = 1.0 }, sortedPoints.[0])
    Assert.Equal({ X = 2.0; Y = 4.0 }, sortedPoints.[1])

[<Fact>]
let ``generateInterpolationPoints with no points returns empty list`` () =
    let state = { 
        Points = []
        LastProcessedX = None
        Method = Linear
        Step = 0.1
    }
    let result = generateInterpolationPoints state (Some 10.0)
    Assert.Empty(result)

[<Fact>]
let ``generateInterpolationPoints with insufficient points for Newton`` () =
    let state = { 
        Points = [ { X = 0.0; Y = 0.0 }; { X = 1.0; Y = 1.0 } ]
        LastProcessedX = None
        Method = Newton 4
        Step = 0.1
    }
    let result = generateInterpolationPoints state (Some 10.0)
    Assert.Empty(result)

[<Fact>]
let ``getMethodName returns correct names`` () =
    Assert.Equal("linear", getMethodName Linear)
    Assert.Equal("newton-3", getMethodName (Newton 3))
    Assert.Equal("newton-5", getMethodName (Newton 5))

[<Fact>]
let ``processNewPoint with linear method`` () =
    let state = { 
        Points = []
        LastProcessedX = None
        Method = Linear
        Step = 0.5
    }
    let point = { X = 1.0; Y = 1.0 }
    let result = processNewPoint [state] point
    let processResult = result |> List.head
    Assert.Equal("linear", processResult.MethodName)
    let expectedPoints = [point]
    Assert.Equal<Point list>(expectedPoints, processResult.State.Points)

[<Fact>]
let ``finalizeProcessing with no points`` () =
    let state = { 
        Points = []
        LastProcessedX = None
        Method = Linear
        Step = 0.5
    }
    let result = finalizeProcessing [state]
    let processResult = result |> List.head
    Assert.Empty(processResult.NewPoints)