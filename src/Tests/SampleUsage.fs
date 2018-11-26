module SampleUsage
// Copied and pasted directly from a unit test,
// so that the compiler can help verify the code
// sample that goes in README.md:
open Plot.Core
open Plot.Core.LineChart
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

let series =
    { originalPoints = Plot.Core.FakeData.monthlyDataDateTimes
      title          = "Air Passenger Data"
      lineStyle      = 
            { Color     = Rgba32.Orange
              Thickness = 2.f }}
let inflatedSeries = 
    { originalPoints = Plot.Core.FakeData.monthlyDataDateTimes |> Array.map(fun op -> { op with y = op.y + 100. })
      title          = "Inflated Air Passenger Data"
      lineStyle      = 
        { Color      = Rgba32.LightBlue
          Thickness = 2.f }}

let settings = Settings.createLineChartSettings "Air Passenger Data Jan 49 – Dec 60" 1500 500

match [series; inflatedSeries] |> createLineChart settings with
| Some image -> image.Save "AirPassengerData.png"
| None   -> failwith "Maybe you provided an empty series of data points."