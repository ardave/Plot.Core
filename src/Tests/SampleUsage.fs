module SampleUsage

open Plot.Core
open Plot.Core.LineChart
open SixLabors.ImageSharp

let series =
    {
        originalPoints = Plot.Core.FakeData.hourlyDataDateTimes
        title = "Air Passenger Data"
        lineStyle = { Color = Rgba32.Orange; Thickness = 2.f }
    }
let inflatedSeries = 
    {
        originalPoints = Plot.Core.FakeData.hourlyDataDateTimes |> Array.map(fun op -> { op with y = op.y + 100. })
        title = "Inflated Air Passenger Data"
        lineStyle = { Color = Rgba32.LightBlue; Thickness = 2.f }
    }
let settings = Settings.createLineChartSettings "Air Passenger Data" 1500 500
let image = [series; inflatedSeries] |> createLineChart settings
image.Save "AirPassengerData.png"