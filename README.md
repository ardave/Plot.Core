# Plot.Core
Cross-Platform Chart Generation Library for .NET Core.

I couldn't find a chart rendering library to run under .NET Core (specifically for, but not limited to, AWS Lambda), so I created this one.

## Usage:
```
let points   = Plot.Core.FakeData.hourlyDataDateTimes
let settings = createLineChartSettings "Air Passenger Data"
let imageOpt = points |> createLineChart settings

match imageOpt with
| Some img -> img.Save "AirPassengerData.png"
| None -> failwith "Maybe you didn't include any points for your chart"
```

And results look like:
![Line chart showing airline passengers over time](https://raw.githubusercontent.com/ardave/Plot.Core/master/AirPassengerData.png)

The rendering settings can be customized by updating the Settings record from the code above, before passing to **createLineChart**.  The Settings record has the structure:
```
open SixLabors.ImageSharp

type RenderingStyle =
| Points
| Lines

type LineStyle = {
    Color     : Rgba32
    Thickness : float32
}

type LineChartSettings = {
    Title          : string
    Width          : int
    Height         : int
    GridColor      : Rgba32
    DataColor      : Rgba32
    RenderingStyle : RenderingStyle
    GridLineStyle  : LineStyle
    DataLineStyle  : LineStyle
}
```