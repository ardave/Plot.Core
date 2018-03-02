namespace Plot.Core

module LineChart =

    open System

    open SixLabors.ImageSharp
    open SixLabors.Shapes
    open SixLabors.Primitives
    open SixLabors.Fonts
    open SixLabors.ImageSharp.Drawing

    open Settings

    // TODO:  Maybe make X value a type that is implicitly convertible
    // to float32, based on the constraints of the float32 function
    type public OriginalPoint<'T> = {
        // Original value for UI/Rendering/Display purposes
        originalX : 'T
        // the actual value used for mathematical comparisons
        // and chart rendering/scaling purposes.
        x : float
        y : float
    }

    let originalToPointF o = PointF(float32 o.x, float32 o.y)

    type internal FittedPoint ={
        fittedX : float
        fittedY : float
    } with member this.ToPointF = PointF(float32 this.fittedX, float32 this.fittedY)

    type internal MinMax<'T> = {
        value : float
        originalValue : 'T
    }

    type internal MinMaxes<'T> = {
        minX : MinMax<'T>
        maxX : MinMax<'T>
        minY : float
        maxY : float
    }

    type ImageMutation = IImageProcessingContext<Rgba32> -> unit
    let internal pointf x y = PointF(x, y)
    let private addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
    let private addLineF (p1:FittedPoint) (p2:FittedPoint) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF)

    let private addLines points (pb:PathBuilder) =
        points
        |> Array.map originalToPointF
        |> Seq.ofArray
        |> pb.AddLines
        |> ignore

    let internal fittedToOriginal f = { x = f.fittedX; y = f.fittedY; originalX = f.fittedX }

    let private addLinesF (points:FittedPoint array) (pb:PathBuilder) =
        let pointFs = points |> Array.map fittedToOriginal
        addLines pointFs pb

    let private addAxes pb (img:Image<_>) =
        let x1 = (float img.Width  * 0.1)
        let y1 = (float img.Height * 0.1)
        let x2 = (float img.Width  * 0.1)
        let y2 = (float img.Height * 0.9)
        let x3 = (float img.Width  * 0.9)
        let y3 = (float img.Height * 0.9)
        let p1 = { x = x1; y = y1; originalX = x1 }
        let p2 = { x = x2; y = y2; originalX = x2 }
        let p3 = { x = x3; y = y3; originalX = x3 }
        pb |> addLines [|p1; p2; p3|]
        p1, p3

    let private pointToMinMax point = { originalValue = point.originalX; value = point.x }

    let internal getMinMaxes (firstPoint:OriginalPoint<'T>) points =
        let initialState =
            {
                minX = 
                    {
                        value = firstPoint.x
                        originalValue = firstPoint.originalX
                    }
                maxX =
                    {
                        value = firstPoint.x
                        originalValue = firstPoint.originalX
                    }
                minY = firstPoint.y
                maxY = firstPoint.y
            }

        points
        |> Array.fold (fun minMaxes point ->
            {
                minX = if point.x < minMaxes.minX.value then pointToMinMax point else minMaxes.minX
                minY = Math.Min(minMaxes.minY, point.y)
                maxX = if point.x > minMaxes.maxX.value then pointToMinMax point else minMaxes.maxX
                maxY = Math.Max(minMaxes.maxY, point.y)
            }) initialState

    let internal fitPointsToGrid upperLeft lowerRight firstPoint points =
        let minMaxes = getMinMaxes firstPoint points
        let pointWidth  = minMaxes.maxX.value - minMaxes.minX.value
        let pointHeight = minMaxes.maxY - minMaxes.minY
        let chartWidth  = lowerRight.x - upperLeft.x
        let chartHeight = lowerRight.y - upperLeft.y

        let fittedPoints =
            points
            |> Array.map(fun point ->
                let pctW = (point.x - minMaxes.minX.value) / pointWidth
                let pctH = (minMaxes.maxY - point.y) / pointHeight
                { 
                    fittedX = pctW * chartWidth + upperLeft.x
                    fittedY = pctH * chartHeight + upperLeft.y
                }
            )
        fittedPoints, minMaxes

    let private drawMaxY minMaxes upperLeft (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let maxYStr = minMaxes.maxY.ToString()
        let pbText2 = PathBuilder()
        let p1 = PointF(float32 upperLeft.x - font.Size * float32 maxYStr.Length, float32 upperLeft.y)
        let p2 = PointF(float32 upperLeft.x, float32 upperLeft.y)
        pbText2.AddLine(p1, p2) |> ignore
        let path = pbText2.Build()
        ctx.DrawText(maxYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

    let private drawMinY minMaxes upperLeft lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let minYStr = minMaxes.minY.ToString()
        let pbText = PathBuilder()
        pbText.AddLine(PointF(float32 upperLeft.x - font.Size * float32 minYStr.Length, float32 lowerRight.y), lowerRight |> originalToPointF) |> ignore
        let path = pbText.Build()
        ctx.DrawText(minYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length))  |> ignore

    let private drawMinX minMaxes upperLeft lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) = 
        let minXStr = minMaxes.minX.originalValue.ToString()
        let pb = PathBuilder()
        let p3 = PointF(float32 upperLeft.x , float32 lowerRight.y + font.Size)
        let p4 = PointF(float32 lowerRight.x, float32 lowerRight.y + font.Size)
        pb.AddLine(p3, p4) |> ignore
        let path = pb.Build()
        ctx.DrawText(minXStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

    let private drawMaxX minMaxes lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let maxXStr = minMaxes.maxX.originalValue.ToString()
        let pbText4 = PathBuilder()
        let p5 = PointF(float32 lowerRight.x - font.Size * float32 maxXStr.Length * 0.5f, float32 lowerRight.y + font.Size)
        let p6 = PointF(float32 lowerRight.x + font.Size * float32 maxXStr.Length * 0.5f, float32 lowerRight.y + font.Size)
        pbText4.AddLine(p5, p6) |> ignore
        let path = pbText4.Build()
        ctx.DrawText(maxXStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

    let private fillBackground (ctx:IImageProcessingContext<Rgba32>) =
        ctx.Fill(Rgba32.White) |> ignore

    let private withPathBuilder f =
        let pb = PathBuilder()
        let result = f()
        let path = pb.Build()
        result, path

    let private drawDataLines settings upperLeft lowerRight firstPoint chartPoints =
        let pb = PathBuilder()
        let fittedPoints, minMaxes = fitPointsToGrid upperLeft lowerRight firstPoint chartPoints
        pb |> addLinesF fittedPoints
        let path = pb.Build()
        let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(settings.DataLineStyle.Color, settings.DataLineStyle.Thickness, path) |> ignore
        drawFunc, minMaxes

    let private drawMajorGridLines settings img =
        let pbAxes = PathBuilder()
        let upperLeft, lowerRight = img |> addAxes pbAxes
        let path = pbAxes.Build()
        let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(settings.GridLineStyle.Color, settings.GridLineStyle.Thickness, path) |> ignore
        upperLeft, lowerRight, drawFunc

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

    let glueMinorGridLinesFunctions maxValue upperLeft lowerRight settings =
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