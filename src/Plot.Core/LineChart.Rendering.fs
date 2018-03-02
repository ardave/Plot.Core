namespace Plot.Core.LineChart
    module internal Rendering =
        open SixLabors.Fonts
        open SixLabors.ImageSharp
        open SixLabors.ImageSharp.Drawing
        open SixLabors.Primitives
        open SixLabors.Shapes
        open Plot.Core
        open LineChart.Calculation

        let internal fittedToOriginal f = { x = f.fittedX; y = f.fittedY; originalX = f.fittedX }
        let originalToPointF o = PointF(float32 o.x, float32 o.y)
        let internal pointf x y = PointF(x, y)
        let internal addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
        let internal addLineF (p1:FittedPoint) (p2:FittedPoint) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF)

        let internal addLines points (pb:PathBuilder) =
            points
            |> Array.map originalToPointF
            |> Seq.ofArray
            |> pb.AddLines
            |> ignore

        let internal addLinesF (points:FittedPoint array) (pb:PathBuilder) =
            let pointFs = points |> Array.map fittedToOriginal
            addLines pointFs pb

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