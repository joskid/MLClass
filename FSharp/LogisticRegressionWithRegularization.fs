module LogisticRegressionWithRegularization

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.0
    1.0 / (1.0 + exp(- (θ * x)))

let J (X, y) λ (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let hθ x = 
        1.0 / (1.0 + exp (- (θ * x)))

    let cost i x =
        y.[i] * log (hθ x) + (1.0 - y.[i]) * log (1.0 - hθ x)

    let alternative1() = 
        -1.0 / m * (X |> Matrix.Σrows cost) + λ / (2.0 * m) * Vector.Σ (θ .^ 2.0)

    let alternative2() = 
        -1.0 / m * (X |> Matrix.Σrows cost) + λ / (2.0 * m) * (θ * θ)

    alternative2()

let innerGradientDescent iterationFunction α maxIterations λ (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let h θ x = 
        1.0 / (1.0 + exp (- (θ * x)))

    let iteration θ =
        θ * (1.0 - α * λ / m) - (α / m) * (X |> Matrix.Σrows (fun i x -> ((h θ x) - y.[i]) * x))
    
    new DenseVector(Matrix.columnCount X, 0.0) :> Vector<float> |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent iterateUntilConvergenceWithIntermediateResults α
