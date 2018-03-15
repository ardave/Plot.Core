# Plot.Core
Cross-Platform Chart Generation Library for .NET Core.

I couldn't find a chart rendering library to run under .NET Core (specifically for, but not limited to, AWS Lambda), so I created this one.

## Usage:
```
open Plot.Core
open Plot.Core.LineChart
open SixLabors.ImageSharp

let series = {
        originalPoints = Plot.Core.FakeData.hourlyDataDateTimes
        title = "Air Passenger Data"
        lineStyle = { Color = Rgba32.Orange; Thickness = 2.f }
    }
let inflatedSeries = {
        originalPoints = Plot.Core.FakeData.hourlyDataDateTimes |> Array.map(fun op -> { op with y = op.y + 100. })
        title = "Inflated Air Passenger Data"
        lineStyle = { Color = Rgba32.LightBlue; Thickness = 2.f }
    }
let settings = Settings.createLineChartSettings "Air Passenger Data" 1500 500
let image = [series; inflatedSeries] |> createLineChart settings
image.Save "AirPassengerData.png"
```

And results look like:
![Line chart showing airline passengers over time](https://raw.githubusercontent.com/ardave/Plot.Core/master/AirPassengerData.png)

The rendering settings can be customized by updating the Settings record from the code above, before passing to **createLineChart**.  The Settings record has the structure:
```
open SixLabors.ImageSharp

type LineChartSettings = {
    Title               : string
    Width               : int
    Height              : int
    Font                : SixLabors.Fonts.Font
    GridLineStyle       : LineStyle
    MinorGridLineStyle  : LineStyle
    HorizontalGridLines : int option
    VerticalGridLines   : int option
}
```