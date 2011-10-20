[<AutoOpen>]
module IterationExtensions

let iterateUntilConvergence iteration initialValue =
    let rec loop t = 
        let nextT = iteration t
        if t <> nextT then
            loop nextT
        else
            nextT
    loop initialValue

let iterateUntilConvergenceWithSteps iteration initialValue =
    let rec loop t = seq {
        yield t
        let nextT = iteration t
        if t <> nextT then
            yield! loop nextT
    }
    loop initialValue

let simplifyIterationSteps maxSteps iterationSteps =
    let n = Array.length iterationSteps
    seq {
        for i in 0 .. (max 1 (n / maxSteps)) .. n-1 do
            yield iterationSteps.[i]
    } |> Array.ofSeq