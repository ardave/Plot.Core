module LineChartTests
open Xunit
open FsUnit.Xunit
open Plot.Core
open Plot.Core.LineChart
open SixLabors.Primitives

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

[<Fact>]
let ``fitPointsToGrid should fit the points correctly``() =
    let upperLeft = pointf 150.f 50.f
    let lowerRight = pointf 1350.f 450.f
    let points = FakeData.hourlyDataDateTimes
    let fittedPoints, _ = fitPointsToGrid upperLeft lowerRight points.[0] points
    
    let fartherLeftThanUpperLeft   p = p.fittedX < upperLeft.X
    let fartherUpThanUpperLeft     p = p.fittedY < upperLeft.Y
    let fartherRightThanLowerRight p = p.fittedX > lowerRight.X
    let fartherDownThanLowerRight  p = p.fittedY > lowerRight.Y

    [
        fartherLeftThanUpperLeft
        fartherUpThanUpperLeft
        fartherRightThanLowerRight
        fartherDownThanLowerRight
    ]
    |> List.iter (fun f ->
        fittedPoints
        |> Array.exists f
        |> should be False
    )
