namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SixLabors.ImageSharp
open Plot.Core
open Plot.Core.LineChart
open Plot.Core.Settings
open Helpers

[<TestClass>]
type LineChartTests () =
    [<TestMethod>]
    member this.``Generate actual image from air passenger data`` () =
        let points   = Plot.Core.FakeData.hourlyDataDateTimes
        let settings = createLineChartSettings "Air Passenger Data" 1500 500
        let imageOpt = points |> createLineChart settings

        match imageOpt with
        | Some img -> img.Save "AirPassengerData.png"
        | None -> failwith "Maybe you didn't include any points for your chart"

    [<TestMethod>]
    member this.``getMinMaxes should get the correct min maxes``() =
        let chartPoints =
            [|
                { originalX = 1.f; x = 1.f; y = 2.f  }
                { originalX = 4.f; x = 4.f; y = 5.f  }
                { originalX = 7.f; x = 7.f; y = 8.f  }
            |]

        let minMaxes = chartPoints |> getMinMaxes chartPoints.[0]

        minMaxes.minX.originalValue |> shouldEqual 1.f
        minMaxes.maxX.originalValue |> shouldEqual 7.f
        minMaxes.minY |> shouldEqual 2.f
        minMaxes.maxY |> shouldEqual 8.f

    [<TestMethod>]
    member this.``fitPointsToGrid should fit the points correctly``() =
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
            |> shouldEqual false
        )
