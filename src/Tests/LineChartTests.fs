namespace Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open SixLabors.Fonts
open SixLabors.ImageSharp
open Plot.Core
open Plot.Core.LineChart.Calculation
open Plot.Core.LineChart.LineChart
open Helpers
open Plot.Core.Settings

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
        let inflatedSeries = 
            {
                originalPoints = Plot.Core.FakeData.hourlyDataDateTimes |> Array.map(fun op -> { op with y = op.y + 100. })
                title = "Inflated Air Passenger Data"
                lineStyle = 
                {
                    Color = Rgba32.LightBlue
                    Thickness = 2.f
                }
            }

        let settings = Settings.createLineChartSettings "Air Passenger Data Jan 49 – Dec 60" 1500 500
        match [series; inflatedSeries] |> createLineChart settings with
        | Some image -> image.Save "AirPassengerData.png"
        | None       -> failwith "Something went wrong."

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
        let axisPoints = AxisPoints.Create { scaledX = 150.; scaledY = 50.} { scaledX = 1350.; scaledY = 450. }
        let series = { originalPoints = FakeData.hourlyDataDateTimes; title = "whatever"; lineStyle = { Color = Rgba32.White; Thickness = 2.f }}
        let scaledSeries, _, _ = scalePointsToGrid axisPoints series.originalPoints.[0] [series]
        
        let fartherLeftThanUpperLeft   p = p.scaledX < axisPoints.upperLeft.scaledX
        let fartherUpThanUpperLeft     p = p.scaledY < axisPoints.upperLeft.scaledY
        let fartherRightThanLowerRight p = p.scaledX > axisPoints.lowerRight.scaledX
        let fartherDownThanLowerRight  p = p.scaledY > axisPoints.lowerRight.scaledY

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

    [<TestMethod>]
    member __.``Axis points should be calculated correctly``() =
        let settings = createLineChartSettings "whatever" 1500 500
        let axisPoints = calculateAxisPoints settings
        let shouldEqualWithinTolerance = shouldEqualWithin 0.001

        axisPoints.upperLeft.scaledX  |> shouldEqualWithinTolerance 150.
        axisPoints.upperLeft.scaledY  |> shouldEqualWithinTolerance 50.
        axisPoints.intersect.scaledX  |> shouldEqualWithinTolerance 150.
        axisPoints.intersect.scaledY  |> shouldEqualWithinTolerance 450.
        axisPoints.lowerRight.scaledX |> shouldEqualWithinTolerance 1350.
        axisPoints.lowerRight.scaledY |> shouldEqualWithinTolerance 450.

    [<TestMethod>]
    member __.``getFontSize should calculate the maximum font size that can fit in the defined space``() =
        let settings = createLineChartSettings "" 1500 500 
        [10; 25; 50; 100; 200]
        |> List.map float
        |> List.iter(fun verticalSpace ->
            let fontSize = getFontSize settings "whatever" verticalSpace
            let font = SystemFonts.CreateFont(settings.Font.Name, fontSize, FontStyle.Regular)
            let size = getSize font "whatever"
            let height = float size.Height
            let proportionFilled = height / verticalSpace

            if height > verticalSpace then
                failwithf "Rendered text height %f was larger than allowed vertical spacing %f" height verticalSpace

            if proportionFilled < 0.75 then
                failwithf "Rendered text should have filled at least 75%% of available vertical space, but %f / %f = %f" height verticalSpace proportionFilled
        )
