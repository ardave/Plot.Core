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
        let drawMajorGridLinesFunc = drawMajorGridLines axisPoints settings
        

        match seriesList |> List.tryHead with
        | None             -> ()
        | Some firstSeries ->
            match firstSeries.originalPoints |> Array.tryGet 0 with
            | None      -> ()
            | Some firstPoint -> 
                let scaledSeriesList, minMaxes, scalingFactors = scalePointsToGrid axisPoints firstPoint seriesList
                let endingY, drawMinXfunc = drawMinX minMaxes axisPoints settings.Font

                let backgroundMutations = [
                    fillBackground
                    drawMajorGridLinesFunc
                    drawTitle settings
                    drawLegend seriesList settings axisPoints
                ]
                let drawSeriesFuncs = scaledSeriesList |> List.map drawDataLines
                let drawMinorGridLinesFunc = assembleMinorGridLinesFunctions settings scalingFactors
                let allMutations = backgroundMutations @ drawSeriesFuncs @ [
                                                drawMaxX minMaxes axisPoints.lowerRight settings.Font
                                                drawMinXfunc
                                                drawMinY minMaxes axisPoints settings.Font
                                                drawMaxY minMaxes axisPoints.upperLeft settings.Font
                                                drawMinorGridLinesFunc ]
                img.Mutate(fun ctx ->
                    allMutations |> List.iter(fun m -> m ctx))
        img

