module MultivariateLinearRegression

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let padVectorWith1InFirstColumn v =
    let paddedV = new DenseVector((Vector.length v) + 1)
    paddedV.Item(0) <- 1.0
    for i in 1 .. v.Count do
        paddedV.Item(i) <- v.Item(i-1)
    paddedV

let h (t:Vector<float>) x = 
    assert (Vector.length t = (Vector.length x) + 1)
    
    t * (padVectorWith1InFirstColumn x)

let J X y t =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float
    let sqr x = x ** 2.0
    1.0/(2.0*m) * (X * t - y |> Vector.map sqr).Sum()

let innerGradientDescent iterationFunction alpha X y =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let n = Matrix.columnCount X
    let m = Matrix.rowCount X |> float

    let iteration t =
        t - (alpha / m) * (X |> Matrix.sumRowsBy (fun i x -> (t * x - y.[i]) * x))
    
    new DenseVector(n, 0.0) :> Vector<float> |> iterationFunction iteration 

let gradientDescent alpha X y = innerGradientDescent iterateUntilConvergence alpha X y
let gradientDescentWithSteps alpha X y = innerGradientDescent iterateUntilConvergenceWithSteps alpha X y
    
let normalEquation X y =
    assert (Matrix.rowCount X = Vector.length y)
    
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))   
    let n = Matrix.columnCount X
    let m = Matrix.rowCount X

    let Xtrans = X.Transpose()
    (Xtrans * X).Inverse() * Xtrans * y

let plotGradientDescentIterations alpha X y =
    (gradientDescentWithSteps alpha X y) 
    |> Seq.map (J X y) 
    |> Array.ofSeq
    |> plotIterations "Gradient Descent Iterations"
