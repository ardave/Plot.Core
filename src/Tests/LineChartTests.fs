module LineChartTests
open Xunit
open FsUnit.Xunit
open Plot.Core.LineChart

[<Fact>]
let ``getMinMaxes should get the correct min maxes``() =
    let chartPoints =
        [|
            { originalX = 1.f; x = 1.f; y = 2.f  }
            { originalX = 4.f; x = 4.f; y = 5.f  }
            { originalX = 7.f; x = 7.f; y = 8.f  }
        |]

    let minMaxes = chartPoints |> getMinMaxes chartPoints.[0]

    minMaxes.minX.originalValue |> should equal 1.f
    minMaxes.maxX.originalValue |> should equal 7.f
    minMaxes.minY |> should equal 2.f
    minMaxes.maxY |> should equal 8.f