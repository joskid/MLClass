#load "Setup.fsx"
#load "LinearRegressionWithOneVariable.fs"

open MathNet.Numerics.LinearAlgebra.Double
open MSDN.FSharp.Charting

// ==================== Part 1: Basic Function ====================

printfn "Running warmUpExercise ..."
printfn "5x5 Identity Matrix:"
    
DenseMatrix.Identity(5) |> printfn "%O" 

// ======================= Part 2: Plotting =======================

printfn "Plotting Data ..."
    
let data = LinearRegressionWithOneVariable.loadData "data/ex1data1.txt"
    
let chartWindow =
    FSharpChart.Point(data, Name = "Training Data")
    |> withRedCrossMarkerStyle
    |> FSharpChart.WithArea.AxisX(Title = "Population of City in 10,000s", Minimum = 4.0)
    |> FSharpChart.WithArea.AxisY(Title = "Profit in $10,000s")
    |> ChartWindow.show "Population of City vs. Profit"
    
// =================== Part 3: Gradient descent ===================

printfn "Running Gradient Descent ..."
    
let iterations = 1500
let α = 0.01
    
let J = (0.0, 0.0) |> LinearRegressionWithOneVariable.J data
printfn "J = %O" J

let θ = data |> LinearRegressionWithOneVariable.gradientDescent α iterations
printfn "θ found by gradient descent: %A" θ

let predictions = [ for (x, _) in data -> (x, LinearRegressionWithOneVariable.h θ x) ]
FSharpChart.Line(predictions, Name = "Linear Regression")
|> FSharpChart.WithLegend()    
|> chartWindow.Combine

// Predict values for population sizes of 35,000 and 70,000
let predict1 = LinearRegressionWithOneVariable.h θ 3.5
printfn "For population = 35,000, we predict a profit of %A" (predict1 * 10000.0)
let predict2 = LinearRegressionWithOneVariable.h θ 7.0
printfn "For population = 70,000, we predict a profit of %A" (predict2 * 10000.0)

// ============= Part 4: Visualizing J(θ0, θ1) =============

printfn "Visualizing J(θ0, θ1) ..."

plotSurface "J surface" (-10.0, -1.0) (10.0, 4.0) (LinearRegressionWithOneVariable.J data)

plotContour "J contour" ("θ0", "θ1") (-10.0, -1.0) (10.0, 4.0) (LinearRegressionWithOneVariable.J data)
