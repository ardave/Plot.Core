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
        x : float32
        y : float32
    } with member this.ToPointF = PointF(this.x, this.y)

    type internal FittedPoint ={
        fittedX : float32
        fittedY : float32
    } with member this.ToPointF = PointF(this.fittedX, this.fittedY)

    
    type internal MinMax<'T> = {
        value : float32
        originalValue : 'T
    }

    type internal MinMaxes<'T> = {
        minX : MinMax<'T>
        maxX : MinMax<'T>
        minY : float32
        maxY : float32
    }

    type ImageMutation = IImageProcessingContext<Rgba32> -> unit
    let internal pointf x y = PointF(x, y)
    let private addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
    let private addLineO (p1:OriginalPoint<'T>) (p2:OriginalPoint<'T>) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF)
    let private addLineF (p1:FittedPoint) (p2:FittedPoint) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF)
    let private pointsToPoints points = points |> Array.map(fun point -> PointF(point.x, point.y))

    let private addLines (points:PointF array) (pb:PathBuilder) =
        pb.AddLines points |> ignore
    let private addLinesF (points:FittedPoint array) (pb:PathBuilder) =
        let pointFs = points |> Array.map (fun p -> p.ToPointF)
        addLines pointFs pb

    let private addAxes pb (img:Image<_>) =
        let x1 = (float32 img.Width  * 0.1f)
        let y1 = (float32 img.Height * 0.1f)
        let x2 = (float32 img.Width  * 0.1f)
        let y2 = (float32 img.Height * 0.9f)
        let x3 = (float32 img.Width  * 0.9f)
        let y3 = (float32 img.Height * 0.9f)
        let p1 = pointf x1 y1
        let p2 = pointf x2 y2
        let p3 = pointf x3 y3
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

    let internal fitPointsToGrid (upperLeft:PointF) (lowerRight:PointF) firstPoint points =
        let minMaxes = getMinMaxes firstPoint points
        let pointWidth  = minMaxes.maxX.value - minMaxes.minX.value
        let pointHeight = minMaxes.maxY - minMaxes.minY
        let chartWidth  = lowerRight.X - upperLeft.X
        let chartHeight = lowerRight.Y - upperLeft.Y

        let fittedPoints =
            points
            |> Array.map(fun point ->
                let pctW = (point.x - minMaxes.minX.value) / pointWidth
                let pctH = (minMaxes.maxY - point.y) / pointHeight
                { 
                    fittedX = pctW * chartWidth + upperLeft.X
                    fittedY = pctH * chartHeight + upperLeft.Y
                }
            )
        fittedPoints, minMaxes

    let private drawMaxY minMaxes (upperLeft:PointF) (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let maxYStr = minMaxes.maxY.ToString()
        let pbText2 = PathBuilder()
        let p1 = PointF(upperLeft.X - font.Size * float32 maxYStr.Length, upperLeft.Y)
        let p2 = PointF(upperLeft.X, upperLeft.Y)
        pbText2.AddLine(p1, p2) |> ignore
        let path = pbText2.Build()
        ctx.DrawText(maxYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

    let private drawMinY minMaxes (upperLeft:PointF) (lowerRight:PointF) (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let minYStr = minMaxes.minY.ToString()
        let pbText = PathBuilder()
        pbText.AddLine(PointF(upperLeft.X - font.Size * float32 minYStr.Length, lowerRight.Y), lowerRight) |> ignore
        let path = pbText.Build()
        ctx.DrawText(minYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length))  |> ignore

    let private drawMinX minMaxes (upperLeft:PointF) (lowerRight:PointF) (font:Font) (ctx:IImageProcessingContext<Rgba32>) = 
        let minXStr = minMaxes.minX.originalValue.ToString()
        let pb = PathBuilder()
        let p3 = PointF(upperLeft.X,  lowerRight.Y + font.Size)
        let p4 = PointF(lowerRight.X, lowerRight.Y + font.Size)
        pb.AddLine(p3, p4) |> ignore
        let path = pb.Build()
        ctx.DrawText(minXStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

    let private drawMaxX minMaxes (lowerRight:PointF) (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let maxXStr = minMaxes.maxX.originalValue.ToString()
        let pbText4 = PathBuilder()
        let p5 = PointF(lowerRight.X - font.Size * float32 maxXStr.Length * 0.5f, lowerRight.Y + font.Size)
        let p6 = PointF(lowerRight.X + font.Size * float32 maxXStr.Length * 0.5f, lowerRight.Y + font.Size)
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

    let drawMinorGridLines =
        ()

    let private drawTitle settings (ctx:IImageProcessingContext<Rgba32>) =
        let pb = PathBuilder()
        pb.ResetOrigin() |> ignore
        let starting = float32 settings.Width / 2.f
        let y = float32 settings.Height * 0.1f
        let titleWidth = float32 settings.Title.Length * settings.Font.Size
        let p5 = PointF(starting - titleWidth * 0.25f, y)
        let p6 = PointF(starting + titleWidth * 0.75f, y)

        pb.AddLine(p5, p6) |> ignore
        pb.ResetOrigin() |> ignore
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

            let allMutations = backgroundMutations @ [
                                            drawDataLinesFunc
                                            drawMaxX minMaxes lowerRight settings.Font
                                            drawMinX minMaxes upperLeft lowerRight settings.Font
                                            drawMinY minMaxes upperLeft lowerRight settings.Font
                                            drawMaxY minMaxes upperLeft settings.Font
                                        ]

            img.Mutate(fun ctx ->
                allMutations |> List.iter(fun m -> m ctx)
            )
            Some img