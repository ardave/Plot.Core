open System.Runtime.CompilerServices
open SixLabors.ImageSharp
open Plot.Core.LineChart.LineChart
open Plot.Core.Settings

[<assembly: InternalsVisibleTo("Tests")>]
do()

[<EntryPoint>]
let main _ =
    let points   = Plot.Core.FakeData.hourlyDataDateTimes
    let settings = createLineChartSettings "Air Passenger Data" 1500 500
    let imageOpt = points |> createLineChart settings

    match imageOpt with
    | Some img -> img.Save "AirPassengerData.png"
    | None -> failwith "Maybe you didn't include any points for your chart"
    0
