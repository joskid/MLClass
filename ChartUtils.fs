[<AutoOpen>]
module ChartUtils

open MSDN.FSharp.Charting

let plotIterations title values =
    values
    |> Seq.mapi (fun i x -> (i, x))
    |> Array.ofSeq
    |> simplifyIterationSteps 200
    |> FSharpChart.Line
    |> FSharpChart.WithArea.AxisX(Minimum = 0.0)
    |> ChartWindow.show title
