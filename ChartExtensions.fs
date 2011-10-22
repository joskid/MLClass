[<AutoOpen>]
module ChartExtensions

open MSDN.FSharp.Charting

let plotIterations title values =
    values
    |> Seq.mapi (fun i x -> (i, x))
    |> Array.ofSeq
    |> simplifyIterationSteps 200
    |> FSharpChart.Line
    |> FSharpChart.WithArea.AxisX(Minimum = 0.0)
    |> ChartWindow.show title

open System.Drawing
open System.Windows.Forms.DataVisualization.Charting
open MSDN.FSharp.Charting.ChartTypes

type SeriesProperties with
    
    member series.Marker<'T when 'T :> GenericChart>(?Style, ?Color, ?Size, ?BorderColor, ?BorderWidth) =
        fun (ch:'T) ->
            Style |> Option.iter ch.Series.set_MarkerStyle
            Color |> Option.iter ch.Series.set_MarkerColor
            Size |> Option.iter ch.Series.set_MarkerSize
            BorderColor |> Option.iter ch.Series.set_MarkerBorderColor
            BorderWidth |> Option.iter ch.Series.set_MarkerBorderWidth
            ch

let withRedCrossMarkerStyle c = c |> FSharpChart.WithSeries.Marker(MarkerStyle.Cross, Color.Red, 10)
