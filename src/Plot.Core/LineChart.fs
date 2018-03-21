namespace Plot.Core.LineChart

[<AutoOpen>]
module LineChart =

    open System
    open SixLabors.ImageSharp
    open Plot.Core
    open Plot.Core.LineChart
    open Plot.Core.Settings

    // let internal assembleMinorGridLinesFunctions settings scalingFactors =
    //     match settings.HorizontalGridLines with
    //     | None -> ignore
    //     | Some numLines ->
    //         getMinorGridLinePoints scalingFactors numLines
    //         |> drawMinorGridLines settings

    let createLineChart settings seriesList =
        let img = new Image<Rgba32>(settings.Width, settings.Height)
        let axisPoints = calculateAxisPoints settings

        match seriesList |> List.tryHead with
        | None             -> None
        | Some firstSeries ->
            match firstSeries.originalPoints |> Array.tryGet 0 with
            | None            -> None
            | Some firstPoint -> 
                let scaledSeriesList, minMaxes, scalingFactors = scalePointsToGrid axisPoints firstPoint seriesList
                let minXPosition = calcMinXPosition minMaxes axisPoints settings.Font
                let maxXPosition = calcMaxXPosition minMaxes settings.Font axisPoints.lowerRight
                let minYPosition = calcMinYPosition minMaxes axisPoints settings.Font
                let maxYPosition = calcMaxYPosition minMaxes axisPoints.upperLeft settings.Font
                
                let img = drawLineChart axisPoints settings seriesList scaledSeriesList scalingFactors minMaxes minXPosition minYPosition maxXPosition maxYPosition
                Some img
