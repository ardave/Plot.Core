open System.Runtime.CompilerServices
open SixLabors.ImageSharp
open SixLabors.Shapes
open SixLabors.Primitives
open SixLabors.Fonts
open SixLabors.ImageSharp.Drawing
open Plot.Core.LineChart
open Plot.Core.Settings

[<assembly: InternalsVisibleTo("tests")>]
do()

[<EntryPoint>]
let main _ =
    let points   = Plot.Core.FakeData.hourlyDataDateTimes
    let settings = createLineChartSettings "Air Passenger Data"
    let imageOpt = points |> createLineChart settings

    match imageOpt with
    | Some img -> img.Save "AirPassengerData.png"
    | None -> failwith "Maybe you didn't include any points for your chart"
    0
