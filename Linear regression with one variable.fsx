#load "Setup.fsx"
#load "LinearRegression.fs"

let intToFloat data = 
    List.map (fun (x, y) -> (float x, float y)) data

let data = [(3, 2)
            (1, 2)
            (0, 1)
            (4, 3)] |> intToFloat

let t = LinearRegression.gradientDescent 0.1 data
LinearRegression.chart t data
LinearRegression.plotGradientDescentIterations 0.1 data