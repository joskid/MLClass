#load "Setup.fsx"
#load "MultivariateLinearRegression.fs"

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.FSharp

let prepare data = 
    data |> List.map (fun (x, _) -> List.map (fun x -> float x) x) |> matrix,
    data |> List.map (fun (_, y) -> float y) |> vector

let X, y = [[2; 5; 1; 5], 460
            [1; 3; 2; 4], 23
            [1; 3; 2; 3], 315
            [8; 2; 1; 3], 178] |> prepare

let t = MultivariateLinearRegression.gradientDescent 0.01 X y
let J = MultivariateLinearRegression.J X y t 
t |> printfn "t = %A"
J |> printfn "J = %A"

MultivariateLinearRegression.plotGradientDescentIterations 0.01 X y

let t2 = MultivariateLinearRegression.normalEquation X y
let J2 = MultivariateLinearRegression.J X y t2 
t2 |> printfn "t2 = %A"
J2 |> printfn "J2 = %A"
