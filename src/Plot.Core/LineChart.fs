namespace Plot.Core

module LineChart =

    open System

    open SixLabors.ImageSharp
    open SixLabors.Shapes
    open SixLabors.Primitives
    open SixLabors.Fonts
    open SixLabors.ImageSharp.Drawing

    open Settings

    type ChartPoint<'T> = {
        // Original value for UI/Rendering/Display purposes
        originalX : 'T
        // the actual value used for mathematical comparisons
        // and chart rendering/scaling purposes.
        x : float32
        y : float32
    } 

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

    let private pointf (x:int) (y:int) = PointF(float32 x, float32 y)
    let private addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
    let private pointsToPoints points = points |> Array.map(fun point -> PointF(point.x, point.y))

    let private addLines (points:PointF array) (pb:PathBuilder) =
        pb.AddLines points |> ignore

    let private addAxes pb (img:Image<_>) =
        let x1 = (float img.Width  * 0.1) |> int
        let y1 = (float img.Height * 0.1) |> int
        let x2 = (float img.Width  * 0.1) |> int
        let y2 = (float img.Height * 0.9) |> int
        let x3 = (float img.Width  * 0.9) |> int
        let y3 = (float img.Height * 0.9) |> int
        let p1 = pointf x1 y1
        let p2 = pointf x2 y2
        let p3 = pointf x3 y3
        pb |> addLines [|p1; p2; p3|]
        p1, p3

    let private pointToMinMax point = { originalValue = point.originalX; value = point.x }

    let internal getMinMaxes (firstPoint:ChartPoint<'T>) points =
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

    let private transformPointsToFitGrid (upperLeft:PointF) (lowerRight:PointF) firstPoint points =
        let minMaxes = getMinMaxes firstPoint points
        let pointWidth  = minMaxes.maxX.value - minMaxes.minX.value
        let pointHeight = minMaxes.maxY - minMaxes.minY
        let chartWidth  = lowerRight.X - upperLeft.X
        let chartHeight = lowerRight.Y - upperLeft.Y

        let transformedPoints =
            points
            |> Array.map(fun point ->
                let pctW = (point.x - minMaxes.minX.value) / pointWidth
                let pctH = (minMaxes.maxY - point.y) / pointHeight
                PointF(pctW * chartWidth + upperLeft.X, pctH * chartHeight + upperLeft.Y)
            )
        transformedPoints, minMaxes

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

    type ImageMutation = IImageProcessingContext<Rgba32> -> unit

    let private fillBackground (ctx:IImageProcessingContext<Rgba32>) =
        ctx.Fill(Rgba32.White) |> ignore

    let private withPathBuilder f =
        let pb = PathBuilder()
        let result = f()
        let path = pb.Build()
        result, path

    let private drawDataLines settings upperLeft lowerRight firstPoint chartPoints =
        let pb = PathBuilder()
        let transformedPoints, minMaxes = transformPointsToFitGrid upperLeft lowerRight firstPoint chartPoints
        pb |> addLines transformedPoints
        let path = pb.Build()
        let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(settings.DataLineStyle.Color, settings.DataLineStyle.Thickness, path) |> ignore
        drawFunc, minMaxes

    let private drawGridLines settings img =
        let pbAxes = PathBuilder()
        let upperLeft, lowerRight = img |> addAxes pbAxes
        let path = pbAxes.Build()
        let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(settings.GridLineStyle.Color, settings.GridLineStyle.Thickness, path) |> ignore
        upperLeft, lowerRight, drawFunc

    let private drawTitle settings (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
        let pb = PathBuilder()
        pb.ResetOrigin() |> ignore
        let starting = float32 settings.Width / 2.f
        let y = float32 settings.Height * 0.1f
        let titleWidth = float32 settings.Title.Length * font.Size
        let p5 = PointF(starting - titleWidth * 0.25f, y)
        let p6 = PointF(starting + titleWidth * 0.75f, y)

        pb.AddLine(p5, p6) |> ignore
        pb.ResetOrigin() |> ignore
        let path = pb.Build()
        ctx.DrawText(settings.Title, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore


    let createLineChart settings (chartPoints:ChartPoint<'T> array) =
        let img = new Image<Rgba32>(settings.Width, settings.Height)
        let font = SystemFonts.CreateFont("Arial", 18.f, FontStyle.Regular)
        let upperLeft, lowerRight, drawGridLinesFunc = drawGridLines settings img

        let backgroundMutations = [
            fillBackground
            drawGridLinesFunc
            drawTitle settings font
        ]

        match chartPoints |> Array.tryGet 0 with
        | None -> None
        | Some firstPoint ->
            let drawDataLinesFunc, minMaxes = drawDataLines settings upperLeft lowerRight firstPoint chartPoints

            let allMutations = backgroundMutations @ [
                                            drawDataLinesFunc
                                            drawMaxX minMaxes lowerRight font
                                            drawMinX minMaxes upperLeft lowerRight font
                                            drawMinY minMaxes upperLeft lowerRight font
                                            drawMaxY minMaxes upperLeft font
                                        ]

            img.Mutate(fun ctx ->
                allMutations |> List.iter(fun m -> m ctx)
            )
            Some img