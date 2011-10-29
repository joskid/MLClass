module SurfacePlotWindow

open System.IO
open System.Windows
open System.Windows.Forms.Integration
open Plot3D
open FsWpf

type SurfacePlotWindow(title, func, obsX, obsY, obsZ) as this =

    inherit FsUiObject<Window>(Path.Combine(__SOURCE_DIRECTORY__, "ChartWindow.xaml"))

    [<DefaultValue>]
    [<UiElement>]
    val mutable chartHost : WindowsFormsHost

    do
        this.UiObject.Title <- title
        let plot3D = new Plot3D(func, obsX, obsY, obsZ)
        this.chartHost.Child <- plot3D


let show title func (obsX, obsY, obsZ) = 
    let surfacePlotWindow = new SurfacePlotWindow(title, func, obsX, obsY, obsZ)
    surfacePlotWindow.UiObject.Show()
    surfacePlotWindow.UiObject.Activate() |> ignore
    surfacePlotWindow

let run title func (obsX, obsY, obsZ) = 
    let surfacePlotWindow = new SurfacePlotWindow(title, func, obsX, obsY, obsZ)
    surfacePlotWindow.UiObject.ShowDialog() |> ignore