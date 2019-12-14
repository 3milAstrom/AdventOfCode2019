module Types
type ComputerState = {state: List<int64>; extraState: Map<int64,int64>; output: List<int64>; relativeBase: int64; index: int64; stop: bool}
type Direction =
    | Up
    | Down
    | Left
    | Right
