module MultivariateLinearRegression

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let padVectorWith1InFirstColumn (v:Vector<float>) =
    let paddedV = new DenseVector(v.Count + 1)
    paddedV.Item(0) <- 1.0
    for i in 1 .. v.Count do
        paddedV.Item(i) <- v.Item(i-1)
    paddedV

let h (t:Vector<float>) x = 
    t * (padVectorWith1InFirstColumn x)

let J t data =    
    let m = List.length data |> float
    let sqr x = x * x
    1.0/(2.0*m) * List.sumBy (fun (x, y) -> sqr (h t x - y)) data

let gradientDescentIteration alpha m data (t:Vector<float>) =
    let newT = new DenseVector(t.Count)
    for i in 0 .. (t.Count - 1) do        
        let parcel (x, y) = 
            let x = padVectorWith1InFirstColumn x
            (t * x - y) * x.Item(i)
        newT.Item(i) <- t.Item(i) - (alpha / m) * List.sumBy parcel data
    newT

let gradientDescent alpha data =
    let m = List.length data |> float    
    let iteration = gradientDescentIteration alpha m data    
    let n = (fst (List.head data)).Count
    let t0 = new DenseVector(n + 1, 0.0)
    iterateUntilConvergence iteration t0

let gradientDescentWithSteps alpha data =    
    let m = List.length data |> float
    let iteration = gradientDescentIteration alpha m data    
    let n = (fst (List.head data)).Count
    let t0 = new DenseVector(n + 1, 0.0)
    iterateUntilConvergenceWithSteps iteration t0

let normalEquation (data:(Vector<float>*float) list) =
    let m = List.length data
    let n = (fst (List.head data)).Count 
    let X = new DenseMatrix(m, n+1)
    let Y = new DenseVector(m)
    data |> List.iteri (fun i (x, y) -> X.SetRow(i, padVectorWith1InFirstColumn x)
                                        Y.Item(i) <- y)
    let Xtrans = X.Transpose()
    (Xtrans * X).Inverse() * Xtrans * Y

let plotGradientDescentIterations alpha data =
    (gradientDescentWithSteps alpha data) 
    |> Seq.map (fun t -> J t data) 
    |> Array.ofSeq
    |> plotIterations "Gradient Descent Iterations"
