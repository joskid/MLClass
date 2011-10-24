module LinearRegression

open System.IO
open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.Statistics

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.0
    θ * x

let J (X, y) θ =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let alternative1() = 
        let sqr x = x ** 2.0
        1.0 / (2.0*m) * Vector.Σ ((X * θ - y) .^ 2.0)

    let alternative2() = 
        let errors = X * θ - y
        1.0 / (2.0*m) * (errors * errors)
    
    alternative2()    

let innerGradientDescent iterationFunction α maxIterations (X, y) =    
    assert (Matrix.rowCount X = Vector.length y)
        
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let iteration θ =
        θ - (α / m) * (X |> Matrix.Σrows (fun i x -> (θ * x - y.[i]) * x))
    
    new DenseVector(Matrix.columnCount X, 0.0) :> Vector<float> |> iterationFunction iteration maxIterations

let gradientDescent α = innerGradientDescent iterateUntilConvergence α
let gradientDescentWithIntermediateResults α = innerGradientDescent iterateUntilConvergenceWithIntermediateResults α

let plotGradientDescentIterations α maxIterations (X, y) =

    let θiterations = 
        gradientDescentWithIntermediateResults α maxIterations (X, y)
        |> Array.ofSeq

    θiterations |> Seq.map (J (X, y))
                |> plotIterations "Gradient Descent Iterations" "Cost J"
                |> ignore

    θiterations.[θiterations.Length - 1]        

let featureNormalize (X: #Matrix<float>) =    
    
    let μ = 
        X.ColumnEnumerator() 
        |> Seq.map (fun (j, col) -> col.Mean()) 
        |> DenseVector.ofSeq
    
    let σ = 
        X.ColumnEnumerator() 
        |> Seq.map (fun (j, col) -> col.StandardDeviation()) 
        |> DenseVector.ofSeq    
    
    let X = 
        X |> Matrix.mapRows (fun i row -> (row - μ) ./ σ)
    
    (X, μ, σ)
    
let normalEquation (X, y) =
    assert (Matrix.rowCount X = Vector.length y)
    
    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))   
    let X' = X.Transpose()

    (X' * X).Inverse() * X' * y

let loadData file =
    let matrix =
        File.ReadLines(__SOURCE_DIRECTORY__ + "/" + file) 
        |> Seq.map (fun line -> line.Split(',') |> Array.map float) 
        |> DenseMatrix.ofSeq
    let m = matrix.RowCount
    let n = matrix.ColumnCount
    (matrix.[0..,..n-2], matrix.Column(n-1))
