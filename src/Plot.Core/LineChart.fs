namespace Plot.Core.LineChart

module LineChart =

    open System

    open SixLabors.ImageSharp
    
    open Calculation
    open Rendering
    open Plot.Core
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
        let upperLeft, lowerRight, drawMajorGridLinesFunc = drawMajorGridLines settings img

        let backgroundMutations = [
            fillBackground
            drawMajorGridLinesFunc
            drawTitle settings
        ]

        seriesList
        |> List.iter(fun series ->
            match series.originalPoints |> Array.tryGet 0 with
            | None -> ()
            | Some firstPoint ->
                let scaledSeries, minMaxes, scalingFactors = scalePointsToGrid upperLeft lowerRight firstPoint seriesList
                let drawDataLinesFunc      = drawDataLines scaledSeries
                let drawMinorGridLinesFunc = assembleMinorGridLinesFunctions settings scalingFactors

                let allMutations = backgroundMutations @ [
                                                drawDataLinesFunc
                                                drawMaxX minMaxes lowerRight settings.Font
                                                drawMinX minMaxes upperLeft lowerRight settings.Font
                                                drawMinY minMaxes upperLeft lowerRight settings.Font
                                                drawMaxY minMaxes upperLeft settings.Font
                                                drawMinorGridLinesFunc
                                            ]

                img.Mutate(fun ctx ->
                    allMutations |> List.iter(fun m -> m ctx)
            )
        )
        img
