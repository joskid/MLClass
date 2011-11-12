module LinearRegressionWithRegularization

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let J (X, y) λ (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let alternative1() = 
        1.0 / (2.0*m) * (Vector.Σ ((X * θ - y) .^ 2.0) + λ * Vector.Σ (θ .^ 2.0))

    let alternative2() = 
        let errors = X * θ - y
        1.0 / (2.0*m) * ((errors * errors) + λ * (θ * θ))
    
    alternative2()    

let innerGradientDescent iterationFunction α maxIterations λ (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let iteration θ =
        θ * (1.0 - α * λ / m) - (α / m) * (X |> Matrix.Σrows (fun i x -> (θ * x - y.[i]) * x))
    
    new DenseVector(Matrix.columnCount X, 0.0) :> Vector<float> |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent iterateUntilConvergenceWithIntermediateResults α

let normalEquation λ (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))   
    let X' = X.Transpose()
    
    (X' * X + λ * DenseMatrix.Identity(X.ColumnCount)).Inverse() * X' * y
