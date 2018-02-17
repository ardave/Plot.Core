# Plot.Core
Cross-Platform Chart Generation Library for .NET Core

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