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
let ``getMinMaxes from air passenger data``() =
    let minMax = FakeData.hourlyDataDateTimes |> getMinMaxes FakeData.hourlyDataDateTimes.[0]
    printfn "minMax: %A" minMax

[<Fact>]
let ``fitPointsToGrid should fit the points correctly``() =
    let upperLeft  = { x = 150.;  y = 50.; originalX  = 150.  }
    let lowerRight = { x = 1350.; y = 450.; originalX = 1350. }
    let points = FakeData.hourlyDataDateTimes
    let fittedPoints, _ = fitPointsToGrid upperLeft lowerRight points.[0] points
    
    let fartherLeftThanUpperLeft   p = p.fittedX < upperLeft.x
    let fartherUpThanUpperLeft     p = p.fittedY < upperLeft.y
    let fartherRightThanLowerRight p = p.fittedX > lowerRight.x
    let fartherDownThanLowerRight  p = p.fittedY > lowerRight.y

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

[<Fact>]
let ``get minor grid lines increment``() =
    let numberOfGridLines = 5
    [|
        7.      , 2.
        1.23    , 0.3
        0.4     , 0.08
        0.00345 , 0.0007
        233.    , 50.
        95300.  , 20000.
    |]
    |> Array.iter(fun (max, expected) ->
        calcMinorGridLineIncrement max numberOfGridLines
        |> should (equalWithin 0.0001) expected
    )

[<Fact>]
let ``get minor grid lines increment from fake data``() =
    let max =
        FakeData.hourlyDataDateTimes
        |> Array.map (fun x -> x.x)
        |> Array.max

    let increment = calcMinorGridLineIncrement max 5
    increment |> shouldEqual 17.
    ()

[<Fact>]
let ``get Minor grid lines points``() = 
    let upperLeft  = { x = 100.; y = 100.; originalX = 100. }
    let lowerRight = { x = 900.; y = 900.; originalX = 900. }
    let numLines   = 5
    let increment  = 10.
    let x = calcMinorGridLinesPoints upperLeft lowerRight numLines increment

    let printATuple (st, nd) = printfn "From (%f, %f) to (%f, %f)" st.x st.y nd.x nd.y
    x |> List.iter printATuple


