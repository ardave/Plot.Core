namespace Plot.Core.LineChart

    [<AutoOpen>]
    module internal Rendering =
        open SixLabors.Fonts
        open SixLabors.ImageSharp
        open SixLabors.ImageSharp.Drawing
        open SixLabors.Primitives
        open SixLabors.Shapes
        open Plot.Core
        open Plot.Core.Settings
        open LineChart.Calculation

        let internal scaledToOriginal f = { x = f.scaledX; y = f.scaledY; originalX = f.scaledX }
        let internal scaledToPointF s = PointF(float32 s.scaledX, float32 s.scaledY)
        let internal originalToPointF o = PointF(float32 o.x, float32 o.y)
        let internal pointf x y = PointF(x, y)
        let internal addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
        let internal addLineF (p1:ScaledPoint) (p2:ScaledPoint) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF)

        let internal addLines points (pb:PathBuilder) =
            points
            |> Array.map originalToPointF
            |> Seq.ofArray
            |> pb.AddLines
            |> ignore

        let internal addLinesF points pb =
            let pointFs = points |> Array.map scaledToOriginal
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

        let internal drawMaxY minMaxes upperLeft (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
            let maxYStr = minMaxes.maxY.ToString()
            let pbText2 = PathBuilder()
            let p1 = PointF(float32 upperLeft.x - font.Size * float32 maxYStr.Length, float32 upperLeft.y)
            let p2 = PointF(float32 upperLeft.x, float32 upperLeft.y)
            pbText2.AddLine(p1, p2) |> ignore
            let path = pbText2.Build()
            ctx.DrawText(maxYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

        let internal drawMinY minMaxes upperLeft lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
            let minYStr = minMaxes.minY.ToString()
            let pbText = PathBuilder()
            pbText.AddLine(PointF(float32 upperLeft.x - font.Size * float32 minYStr.Length, float32 lowerRight.y), lowerRight |> originalToPointF) |> ignore
            let path = pbText.Build()
            ctx.DrawText(minYStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length))  |> ignore

        let internal drawMinX minMaxes upperLeft lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) = 
            let minXStr = minMaxes.minX.originalValue.ToString()
            let pb = PathBuilder()
            let p3 = PointF(float32 upperLeft.x , float32 lowerRight.y + font.Size)
            let p4 = PointF(float32 lowerRight.x, float32 lowerRight.y + font.Size)
            pb.AddLine(p3, p4) |> ignore
            let path = pb.Build()
            ctx.DrawText(minXStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

        let internal drawMaxX minMaxes lowerRight (font:Font) (ctx:IImageProcessingContext<Rgba32>) =
            let maxXStr = minMaxes.maxX.originalValue.ToString()
            let pbText4 = PathBuilder()
            let p5 = PointF(float32 lowerRight.x - font.Size * float32 maxXStr.Length * 0.5f, float32 lowerRight.y + font.Size)
            let p6 = PointF(float32 lowerRight.x + font.Size * float32 maxXStr.Length * 0.5f, float32 lowerRight.y + font.Size)
            pbText4.AddLine(p5, p6) |> ignore
            let path = pbText4.Build()
            ctx.DrawText(maxXStr, font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore

        let internal fillBackground (ctx:IImageProcessingContext<Rgba32>) =
            ctx.Fill(Rgba32.White) |> ignore

        let internal withPathBuilder f =
            let pb = PathBuilder()
            let result = f()
            let path = pb.Build()
            result, path

        let internal drawDataLines scaledSeries =
            let pb = PathBuilder()
            pb |> addLinesF scaledSeries.scaledPoints
            let path = pb.Build()
            let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(scaledSeries.lineStyle.Color, scaledSeries.lineStyle.Thickness, path) |> ignore
            drawFunc

        let internal drawMajorGridLines settings img =
            let pbAxes = PathBuilder()
            let upperLeft, lowerRight = img |> addAxes pbAxes
            let path = pbAxes.Build()
            let drawFunc (ctx:IImageProcessingContext<Rgba32>) = ctx.Draw(settings.GridLineStyle.Color, settings.GridLineStyle.Thickness, path) |> ignore
            upperLeft, lowerRight, drawFunc

        let internal drawMinorGridLines settings minorGridLines (ctx:IImageProcessingContext<Rgba32>) =
            minorGridLines
            |> List.iter (fun line ->
                let pb = PathBuilder()
                pb.AddLine(line.start.ToPointF, line.endd.ToPointF) |> ignore
                ctx.Draw(settings.MinorGridLineStyle.Color, settings.MinorGridLineStyle.Thickness, pb.Build()) |> ignore)

        let internal drawTitle settings (ctx:IImageProcessingContext<Rgba32>) =
            let pb = PathBuilder()
            let pStart, pEnd = calculateTitleLocation settings
            pb.AddLine(pStart, pEnd) |> ignore
            let path = pb.Build()
            ctx.DrawText(settings.Title, settings.Font, Rgba32.Black, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore
