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

    let drawLegend seriesList (ctx:IImageProcessingContext<Rgba32>) = ()

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
                let scaledSeriesList, minMaxes, scalingFactors = scalePointsToGrid upperLeft lowerRight firstPoint seriesList
                let drawSeriesFuncs = scaledSeriesList |> List.map drawDataLines
                let drawMinorGridLinesFunc = assembleMinorGridLinesFunctions settings scalingFactors
                let allMutations = backgroundMutations @ drawSeriesFuncs @[
                                                drawMaxX minMaxes lowerRight settings.Font
                                                drawMinX minMaxes upperLeft lowerRight settings.Font
                                                drawMinY minMaxes upperLeft lowerRight settings.Font
                                                drawMaxY minMaxes upperLeft settings.Font
                                                drawMinorGridLinesFunc
                                                drawLegend seriesList
                ] 
                
                img.Mutate(fun ctx ->
                    allMutations |> List.iter(fun m -> m ctx)
            )
        )
        img
