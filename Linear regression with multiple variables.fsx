#load "Setup.fsx"
#load "MultivariateLinearRegression.fs"

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let intToFloat data = 
    let innerIntToFloat data = 
        List.map (fun x -> float x) data
    List.map (fun (x, y) -> (new DenseVector(Array.ofList (innerIntToFloat x)) :> Vector<float>, float y)) data

let data = [[2; 5; 1; 5], 460
            [1; 3; 2; 4], 23
            [1; 3; 2; 3], 315
            [8; 2; 1; 3], 178] |> intToFloat

let t = MultivariateLinearRegression.gradientDescent 0.01 data
t |> printfn "t = %A"

MultivariateLinearRegression.J t data |> printfn "J = %A"

//MultivariateLinearRegression.plotGradientDescentIterations 0.01 data

//TODO: this should give the same result than gradient descent but it's not :(
let t2 = MultivariateLinearRegression.normalEquation data
t2 |> printfn "t = %A"

MultivariateLinearRegression.J t2 data |> printfn "J = %A"
