module LinearRegression

let h (t0, t1) x = t0 + t1 * x

let J data t =
    let m = List.length data |> float
    1.0/(2.0*m) * List.sumBy (fun (x, y) -> (h t x - y) ** 2.0) data
    
let innerGradientDescent iterationFunction alpha data =
    let m = List.length data |> float    
    let iteration t = 
        (fst t - (alpha / m) * List.sumBy (fun (x, y) -> h t x - y) data,
         snd t - (alpha / m) * List.sumBy (fun (x, y) -> (h t x - y) * x) data)   
    (0.0, 0.0) |> iterationFunction iteration 

let gradientDescent = innerGradientDescent iterateUntilConvergence
let gradientDescentWithSteps = innerGradientDescent iterateUntilConvergenceWithSteps

let plotGradientDescentIterations alpha data =
    (gradientDescentWithSteps alpha data) 
    |> Seq.map (J data) 
    |> Array.ofSeq
    |> plotIterations "Gradient Descent Iterations"
                             
open MSDN.FSharp.Charting
                             
let chart t (data : (float*float) list) =
    FSharpChart.Combine
        [ FSharpChart.Point(data) |> withRedCrossMarkerStyle
          FSharpChart.Line([ for (x, y) in data -> (x, h t x) ]) ]
    |> ChartWindow.show (J data t |> sprintf "Linear Regression (J=%A)")
