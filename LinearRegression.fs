module LinearRegression

let h (t0, t1) x = t0 + t1 * x

let J t data =
    let m = List.length data |> float
    let sqr x = x * x
    1.0/(2.0*m) * List.sumBy (fun (x, y) -> sqr (h t x - y)) data
    
let gradientDescentIteration alpha m data t =
    (fst t - (alpha / m) * List.sumBy (fun (x, y) -> h t x - y) data,
     snd t - (alpha / m) * List.sumBy (fun (x, y) -> (h t x - y) * x) data)

let gradientDescent alpha data =
    let m = List.length data |> float    
    let iteration = gradientDescentIteration alpha m data    
    iterateUntilConvergence iteration (0.0, 0.0)

let gradientDescentWithSteps alpha data =
    let m = List.length data |> float    
    let iteration = gradientDescentIteration alpha m data    
    iterateUntilConvergenceWithSteps iteration (0.0, 0.0)

let plotGradientDescentIterations alpha data =
    (gradientDescentWithSteps alpha data) 
    |> Seq.map (fun t -> J t data) 
    |> Array.ofSeq
    |> plotIterations  "Gradient Descent Iterations"
                             
open MSDN.FSharp.Charting
                             
let chart t (data : (float*float) list) =
    FSharpChart.Combine
        [ FSharpChart.Point(data)
          FSharpChart.Line([ for (x, y) in data -> (x, h t x) ]) ]
    |> ChartWindow.show (J t data |> sprintf "Linear Regression (J=%A)")
