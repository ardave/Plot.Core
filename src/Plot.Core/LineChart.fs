namespace Plot.Core.LineChart

module LineChart =

    open System

    open SixLabors.ImageSharp
    open SixLabors.Shapes
    open SixLabors.Primitives
    open SixLabors.Fonts
    open SixLabors.ImageSharp.Drawing

    open Calculation
    open Rendering
    open Plot.Core
    open Plot.Core.LineChart.Rendering
    open Plot.Core.Settings

    let internal getMinorGridLinePoints maxValue numLines upperLeft lowerRight =
        calcMinorGridLineIncrement maxValue numLines
        |> calcMinorGridLinesPoints upperLeft lowerRight numLines

    let internal glueMinorGridLinesFunctions maxValue upperLeft lowerRight settings scalingFactors =
        match settings.XAxisGridLines with
        | None -> ignore
        | Some numLines ->
            getMinorGridLinePoints maxValue numLines upperLeft lowerRight scalingFactors
            |> drawMinorGridLines settings

    let createLineChart settings (chartPoints:OriginalPoint<'T> array) =
        let img = new Image<Rgba32>(settings.Width, settings.Height)
        let upperLeft, lowerRight, drawMajorGridLinesFunc = drawMajorGridLines settings img

        let backgroundMutations = [
            fillBackground
            drawMajorGridLinesFunc
            drawTitle settings
        ]

        match chartPoints |> Array.tryGet 0 with
        | None -> None
        | Some firstPoint ->
            let scaledPoints, minMaxes, scalingFactors = scalePointsToGrid upperLeft lowerRight firstPoint chartPoints
            let drawDataLinesFunc      = drawDataLines settings scaledPoints
            let drawMinorGridLinesFunc = glueMinorGridLinesFunctions minMaxes.maxX.value upperLeft lowerRight settings scalingFactors

            let allMutations = backgroundMutations @ [
                                            drawDataLinesFunc
                                            drawMaxX minMaxes lowerRight settings.Font
                                            drawMinX minMaxes upperLeft lowerRight settings.Font
                                            drawMinY minMaxes upperLeft lowerRight settings.Font
                                            drawMaxY minMaxes upperLeft settings.Font
                                            // drawMinorGridLinesFunc
                                        ]

            img.Mutate(fun ctx ->
                allMutations |> List.iter(fun m -> m ctx)
            )
            Some img