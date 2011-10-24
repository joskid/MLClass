module LinearRegressionWithOneVariable

let h (θ0, θ1) x = θ0 + θ1 * x

let J data θ =
    let m = List.length data |> float
    1.0 / (2.0*m) * List.sumBy (fun (x, y) -> (h θ x - y) ** 2.0) data
    
let private innerGradientDescent iterationFunction α maxIterations data =
    let m = List.length data |> float    
    
    let iteration θ = 
        (fst θ - (α / m) * List.sumBy (fun (x, y) -> h θ x - y) data,
         snd θ - (α / m) * List.sumBy (fun (x, y) -> (h θ x - y) * x) data)

    (0.0, 0.0) |> iterationFunction iteration maxIterations

let gradientDescent = innerGradientDescent iterateUntilConvergence
let gradientDescentWithIntermediateResults = innerGradientDescent iterateUntilConvergenceWithIntermediateResults

let plotGradientDescentIterations α maxIterations data =
    
    let θiterations = 
        gradientDescentWithIntermediateResults α maxIterations data
        |> Array.ofSeq

    θiterations |> Seq.map (J data)
                |> plotIterations "Gradient Descent Iterations" "Cost J"
                |> ignore

    θiterations.[θiterations.Length - 1]  
                                 
open MSDN.FSharp.Charting
                             
let plot θ (data : (float*float) list) =
    FSharpChart.Combine
        [ FSharpChart.Point(data, Name = "Training Data") |> withRedCrossMarkerStyle
          FSharpChart.Line([ for (x, _) in data -> (x, h θ x) ], Name = "Linear Regression") ]

open System.IO

let loadData file =
    let loadLine (line: string) =
        let parts = line.Split(',') 
        assert (parts.Length = 2)
        (float parts.[0], float parts.[1])
    File.ReadLines(__SOURCE_DIRECTORY__ + "/" + file) 
    |> Seq.map loadLine
    |> List.ofSeq
