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