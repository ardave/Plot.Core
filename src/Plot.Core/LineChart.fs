namespace Plot.Core.LineChart

[<AutoOpen>]
module LineChart =

    open System
    open SixLabors.ImageSharp
    open Plot.Core
    open Plot.Core.LineChart
    open Plot.Core.Settings

    let internal getMinorGridLinePoints scalingFactors numLines =
        let horizontalLines =
            calcMinorGridLineIncrement scalingFactors.pointHeight numLines
            |> calcMinorHorizontalGridLinesPoints numLines scalingFactors

        let verticalLines =
            calcMinorGridLineIncrement scalingFactors.pointWidth numLines
            |> calcMinorVerticalGridLinesPoints numLines scalingFactors

        horizontalLines @ verticalLines

    let internal assembleMinorGridLinesFunctions settings scalingFactors =
        match settings.HorizontalGridLines with
        | None -> ignore
        | Some numLines ->
            getMinorGridLinePoints scalingFactors numLines
            |> drawMinorGridLines settings

    let createLineChart settings seriesList =
        let img = new Image<Rgba32>(settings.Width, settings.Height)
        let axisPoints = calculateAxisPoints settings

        img.Mutate(fun ctx ->
            fillBackground ctx
            drawMajorGridLines axisPoints settings ctx
            drawTitle settings ctx
            drawLegend seriesList settings axisPoints ctx
            )

        match seriesList |> List.tryHead with
        | None             -> ()
        | Some firstSeries ->
            match firstSeries.originalPoints |> Array.tryGet 0 with
            | None            -> ()
            | Some firstPoint -> 
                let scaledSeriesList, minMaxes, scalingFactors = scalePointsToGrid axisPoints firstPoint seriesList
                let endingY, drawMinXfunc = drawMinX minMaxes axisPoints settings.Font

                img.Mutate(fun ctx ->
                    assembleMinorGridLinesFunctions settings scalingFactors ctx
                    scaledSeriesList |> List.iter(fun x -> drawDataLines x ctx)
                    drawMaxX minMaxes axisPoints.lowerRight settings.Font ctx
                    drawMinXfunc ctx
                    drawMinY minMaxes axisPoints settings.Font ctx
                    drawMaxY minMaxes axisPoints.upperLeft settings.Font ctx
                    )
        img

