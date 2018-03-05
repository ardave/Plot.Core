module LineChartTests
open Xunit
open FsUnit.Xunit
open Plot.Core
open Plot.Core.LineChart.Calculation
open Plot.Core.LineChart.LineChart
open Plot.Core.LineChart.Rendering

let shouldEqual expected actual =
    if actual <> expected then
        failwithf "Expected %A but was %A" expected actual

[<Fact>]
let ``getMinMaxes should get the correct min maxes``() =
    let chartPoints =
        [|
            { originalX = 1.f; x = 1.; y = 2. }
            { originalX = 4.f; x = 4.; y = 5. }
            { originalX = 7.f; x = 7.; y = 8. }
        |]

    
    let minMaxes = chartPoints |> getMinMaxes chartPoints.[0]

    minMaxes.minX.originalValue |> shouldEqual 1.f
    minMaxes.minX.originalValue |> shouldEqual 1.f
    minMaxes.maxX.originalValue |> shouldEqual 7.f
    minMaxes.minY |> shouldEqual 2.
    minMaxes.maxY |> shouldEqual 8.

[<Fact>]
let ``scalePointsToGrid should fit the points correctly``() =
    let upperLeft  = { x = 150.;  y = 50.; originalX  = 150.  }
    let lowerRight = { x = 1350.; y = 450.; originalX = 1350. }
    let points = FakeData.hourlyDataDateTimes
    let scaledPoints, _, _ = scalePointsToGrid upperLeft lowerRight points.[0] points
    
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
        scaledPoints
        |> Array.exists f
        |> should be False
    )
