module LogisticRegression

open System;
open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let h (θ: Vector<float>) x = 
    let n = Vector.length x
    assert (Vector.length θ = n + 1)
    let x = x |> Vector.insert 0 1.0
    1.0 / (1.0 + exp(- (θ * x)))

let J (X, y) (θ: Vector<float>) =
    assert (Matrix.rowCount X = Vector.length y)

    let X = X.InsertColumn(0, new DenseVector(Matrix.rowCount X, 1.0))
    let m = Matrix.rowCount X |> float

    let cost i x = 
        if y.[i] = 1.0 then 
            - log(x * θ)
        else
            - log(1.0 - x * θ)

    1.0 / m * Matrix.Σrows cost X
