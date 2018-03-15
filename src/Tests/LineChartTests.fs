namespace Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open SixLabors.ImageSharp
open Plot.Core
open Plot.Core.LineChart.Calculation
open Plot.Core.LineChart.LineChart
open Helpers

[<TestClass>]
type LineChartTests() =
    [<TestMethod>]
    member __.``Generate actual image from air passenger data`` () =
        let series =
            {
                originalPoints = Plot.Core.FakeData.hourlyDataDateTimes
                title = "Air Passenger Data"
                lineStyle = 
                    {
                        Color = Rgba32.Orange
                        Thickness = 2.f
                    }
            }    
        let settings = Settings.createLineChartSettings "Air Passenger Data" 1500 500
        let image = [series] |> createLineChart settings
        image.Save "AirPassengerData.png"

    [<TestMethod>]
    member __.``getMinMaxes should get the correct min maxes``() =
        let chartPoints =
            [|
                { originalX = 1.f; x = 1.; y = 2. }
                { originalX = 4.f; x = 4.; y = 5. }
                { originalX = 7.f; x = 7.; y = 8. }
            |]

        let series = 
            {
                originalPoints = chartPoints
                lineStyle = 
                    {
                        Color = Rgba32.White
                        Thickness = 1.f
                    }
                title = ""
            }

        
        let minMaxes = [series] |> getMinMaxes chartPoints.[0]

        minMaxes.minX.originalValue |> shouldEqual 1.f
        minMaxes.minX.originalValue |> shouldEqual 1.f
        minMaxes.maxX.originalValue |> shouldEqual 7.f
        minMaxes.minY |> shouldEqual 2.
        minMaxes.maxY |> shouldEqual 8.

    [<TestMethod>]
    member __.``scalePointsToGrid should fit the points correctly``() =
        let upperLeft  = { x = 150.;  y = 50.; originalX  = 150.  }
        let lowerRight = { x = 1350.; y = 450.; originalX = 1350. }
        let series = { originalPoints = FakeData.hourlyDataDateTimes; title = "whatever"; lineStyle = { Color = Rgba32.White; Thickness = 2.f }}
        let scaledSeries, _, _ = scalePointsToGrid upperLeft lowerRight series.originalPoints.[0] [series]
        
        let fartherLeftThanUpperLeft   p = p.scaledX < upperLeft.x
        let fartherUpThanUpperLeft     p = p.scaledY < upperLeft.y
        let fartherRightThanLowerRight p = p.scaledX > lowerRight.x
        let fartherDownThanLowerRight  p = p.scaledY > lowerRight.y

        [
            fartherLeftThanUpperLeft
            fartherUpThanUpperLeft
            fartherRightThanLowerRight
            fartherDownThanLowerRight
        ]
        |> List.iter (fun f ->
            (scaledSeries |> List.head).scaledPoints
            |> Array.exists f
            |> shouldEqual false
        )
