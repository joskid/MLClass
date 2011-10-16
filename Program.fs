open System
open MSDN.FSharp.Charting

[<STAThread>]
do 
    let chart =
        FSharpChart.Combine
            [ FSharpChart.Line([ for f in 0.0 .. 0.1 .. 10.0 -> sin f ], Name = "Sin Wave")
              FSharpChart.Line([ for f in 0.0 .. 0.1 .. 10.0 -> cos f ], Name = "Cosine Wave") ]
    ChartWindow.run "Sin/Cosine" chart
