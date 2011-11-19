﻿module LinearRegressionWithRegularization

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let J (X, y) λ (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.))
    let m = Matrix.rowCount X |> float

    let θforReg = θ.Clone()
    θforReg.[0] <- 0.

    let errors = X * θ - y
    1. / (2. * m) * ((errors * errors) + λ * (θforReg * θforReg))    

let innerGradientDescent iterationFunction α maxIterations (λ: float) (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.))
    let m = Matrix.rowCount X |> float

    let iteration (θ: Vector<float>) =

        let θforReg = θ.Clone()
        θforReg.[0] <- 0.

        let errors = X.Transpose() * (X * θ - y)
        θ - (α / m) * errors - λ / m * θforReg

    new DenseVector(Matrix.columnCount X, 0.0) :> Vector<float> |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent iterateUntilConvergenceWithIntermediateResults α

let normalEquation λ (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.))   
    let X' = X.Transpose()
    
    (X' * X + λ * DenseMatrix.Identity(X.ColumnCount)).Inverse() * X' * y
