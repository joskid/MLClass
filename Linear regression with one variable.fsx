#load "Setup.fsx"
#load "LinearRegression.fs"

let prepare data = 
    List.map (fun (x, y) -> (float x, float y)) data

let data = [(3, 2)
            (1, 2)
            (0, 1)
            (4, 3)] |> prepare

let t = LinearRegression.gradientDescent 0.1 data
let J = LinearRegression.J data t 
t |> printfn "t = %A"
J |> printfn "J = %A"
LinearRegression.chart t data

LinearRegression.plotGradientDescentIterations 0.1 data