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

    let internal calcMinorGridLineIncrement maxValue numGridLines =
        let rec getIncrement input multiplier =
            match input with
            | i when i > 10. ->
                getIncrement (i / 10.) (multiplier * 10.)
            | i when i <= 1. ->
                getIncrement (i * 10.) (multiplier / 10.)
            | _ -> Math.Ceiling(float input) * multiplier
        getIncrement (maxValue / float numGridLines) 1.

    let internal getMinorGridLinesPoints upperLeft lowerRight increment numLines =
        [0.. numLines]
        |> List.map(fun n ->
            let x1 = upperLeft.x
            let y1 = lowerRight.y - increment * float n
            let x2 = lowerRight.x
            let y2 = y1
            let newStart = { x = x1; y = y1; originalX = x1 }
            let newEnd   = { x = x2; y = y2; originalX = x2 }
            newStart, newEnd
        )

    let internal drawMinorGridLines minorGridLinesEndPoints settings (ctx:IImageProcessingContext<Rgba32>) =
        let pb = PathBuilder()
        minorGridLinesEndPoints
        |> List.map (fun (x, y) ->
            printfn "x: %A, y: %A" x y
            originalToPointF x, originalToPointF y)
        |> List.iter (fun (x, y) -> pb.AddLine(x, y) |> ignore)
        ctx.Draw(settings.DataLineStyle.Color, settings.DataLineStyle.Thickness, pb.Build()) |> ignore

    let internal glueMinorGridLinesFunctions maxValue upperLeft lowerRight settings =
        match settings.XAxisGridLines with
        | None -> ignore
        | Some numLines ->
            let increment = calcMinorGridLineIncrement maxValue numLines
            let minorGridLinePoints = getMinorGridLinesPoints upperLeft lowerRight increment numLines
            drawMinorGridLines minorGridLinePoints settings

    let internal calculateTitleLocation settings =
        let starting = float32 settings.Width / 2.f
        let y = float32 settings.Height * 0.1f
        let titleWidth = float32 settings.Title.Length * settings.Font.Size
        let pStart = PointF(starting - titleWidth * 0.25f, y)
        let pEnd   = PointF(starting + titleWidth * 0.75f, y)
        pStart, pEnd

    let private drawTitle settings (ctx:IImageProcessingContext<Rgba32>) =
        let pb = PathBuilder()
        let pStart, pEnd = calculateTitleLocation settings
        pb.AddLine(pStart, pEnd) |> ignore
        let path = pb.Build()
        ctx.DrawText(settings.Title, settings.Font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

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
            let drawDataLinesFunc, minMaxes = drawDataLines settings upperLeft lowerRight firstPoint chartPoints
            let drawMinorGridLinesFunc = glueMinorGridLinesFunctions minMaxes.maxX.value upperLeft lowerRight settings

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
            Some img