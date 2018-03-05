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

// not really calculating the increment right now, so no point in testing, yet.
// [<Fact>]
// let ``get minor grid lines increment``() =
    // not really calculating the increment right now, so no point in testing, yet.

    // let upperLeft = OriginalPoint.create 150. 150. 
    // let lowerRight = OriginalPoint.create 1350. 1350. 
    // let minMaxes = minMaxesCreate 150. 150. 1350. 1350.
    // let sf = calculateScalingFactors upperLeft lowerRight 

    // let numberOfGridLines = 5
    // [|
    //     7.      , 2.
    //     1.23    , 0.3
    //     0.4     , 0.08
    //     0.00345 , 0.0007
    //     233.    , 50.
    //     95300.  , 20000.
    // |]
    // |> Array.iter(fun (pointsSpan, expected) ->
    //     let upperLeft = OriginalPoint.Create 0. 0.
    //     let lowerRight = OriginalPoint.Create pointsSpan pointsSpan
    //     calcMinorGridLineIncrement sf numberOfGridLines
    //     |> should (equalWithin 0.0001) expected
    // )

// not really calculating the increment right now, so no point in testing, yet.
// [<Fact>]
// let ``get minor grid lines increment from fake data``() =
//     let max =
//         FakeData.hourlyDataDateTimes
//         |> Array.map (fun x -> x.y)
//         |> Array.max

//     let increment = calcMinorGridLineIncrement max 5
//     increment |> shouldEqual 200.
//     ()

[<Fact>]
let ``get Minor grid lines points``() = 
    let upperLeft  = { x = 100.; y = 100.; originalX = 100. }
    let lowerRight = { x = 900.; y = 900.; originalX = 900. }
    let numLines   = 5
    let increment  = 10.
    calcMinorGridLinesPoints upperLeft lowerRight numLines increment
    |> ignore
    // TODO:  Add meaningful assertion
