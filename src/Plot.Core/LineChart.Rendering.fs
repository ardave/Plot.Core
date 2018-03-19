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

        let private drawText (text:string) (font:Font) (color:Rgba32) (path:IPath) (ctx:IImageProcessingContext<Rgba32>) = 
            ctx.DrawText(text, font, color, path, TextGraphicsOptions(true, WrapTextWidth = path.Length)) |> ignore
        let private draw (color:Rgba32) thickness (path:IPath) (ctx:IImageProcessingContext<Rgba32>) =
            ctx.Draw(color, thickness, path) |> ignore

        let fill (color:Rgba32) (ctx:IImageProcessingContext<Rgba32>) = ctx.Fill(color) |> ignore

        let internal scaledToOriginal f = { x = f.scaledX; y = f.scaledY; originalX = f.scaledX }
        let internal scaledToPointF s = PointF(float32 s.scaledX, float32 s.scaledY)
        let internal originalToPointF o = PointF(float32 o.x, float32 o.y)
        let internal pointf x y = PointF(x, y)
        let internal addLine p1 p2 (pb:PathBuilder) = pb.AddLine(p1, p2) |> ignore
        let internal addLineF (p1:ScaledPoint) (p2:ScaledPoint) (pb:PathBuilder) = pb.AddLine(p1.ToPointF, p2.ToPointF) |> ignore
        let private getSize font text = TextMeasurer.Measure(text, new RendererOptions(font))

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

        let internal drawMaxY minMaxes upperLeft font ctx =
            let spacing = 3.f
            let maxYStr = minMaxes.maxY.ToString()
            let size = getSize font maxYStr
            let pbText2 = PathBuilder()
            let startX = float32 upperLeft.scaledX - (size.Width + spacing)
            let endX = float32 upperLeft.scaledX
            let y = float32 upperLeft.scaledY + (size.Height / 2.f)
            let p1 = PointF(startX, y)
            let p2 = PointF(endX, y)
            pbText2.AddLine(p1, p2) |> ignore
            let path = pbText2.Build()
            drawText maxYStr font Rgba32.Black path ctx

        let internal drawMinY minMaxes axisPoints font ctx =
            let spacing = 3.f
            let minYStr = minMaxes.minY.ToString()
            let size    = getSize font minYStr
            let pbText  = PathBuilder()
            let startX  = float32 axisPoints.upperLeft.scaledX - (size.Width + spacing)
            let endX    = float32 axisPoints.lowerRight.scaledX
            let y       = float32 axisPoints.lowerRight.scaledY - size.Height / 2.f
            pbText.AddLine(PointF(startX, y), PointF(endX, y)) |> ignore
            let path = pbText.Build()
            drawText minYStr font Rgba32.Black path ctx

        let internal drawMinX minMaxes axisPoints font =
            let minXStr = minMaxes.minX.originalValue.ToString()
            let size = getSize font minXStr
            let pb = PathBuilder()
            let p3 = PointF(float32 axisPoints.upperLeft.scaledX , float32 axisPoints.lowerRight.scaledY + size.Height / 2.f)
            let p4 = PointF(float32 axisPoints.lowerRight.scaledX, float32 axisPoints.lowerRight.scaledY + size.Height / 2.f)
            pb.AddLine(p3, p4) |> ignore
            let path = pb.Build()
            let endingY = axisPoints.lowerRight.scaledY + float size.Height
            let f = drawText minXStr font Rgba32.Black path
            endingY, f

        let internal drawMaxX minMaxes lowerRight font ctx =
            let maxXStr = minMaxes.maxX.originalValue.ToString()
            let size = getSize font maxXStr
            let pbText4 = PathBuilder()
            let p5 = PointF(float32 lowerRight.scaledX - size.Width, float32 lowerRight.scaledY + size.Height / 2.f)
            let p6 = PointF(float32 lowerRight.scaledX + size.Width, float32 lowerRight.scaledY + size.Height / 2.f)
            pbText4.AddLine(p5, p6) |> ignore
            let path = pbText4.Build()
            drawText maxXStr font Rgba32.Black path ctx

        let internal fillBackground ctx = fill Rgba32.White ctx

        let internal withPathBuilder f =
            let pb = PathBuilder()
            let result = f()
            let path = pb.Build()
            result, path

        let internal drawDataLines scaledSeries =
            let pb = PathBuilder()
            pb |> addLinesF scaledSeries.scaledPoints
            let path = pb.Build()
            let drawFunc ctx = draw scaledSeries.lineStyle.Color scaledSeries.lineStyle.Thickness path ctx
            drawFunc

        let internal drawMajorGridLines axisPoints settings =
            let pb = PathBuilder()
            pb |> addLineF axisPoints.upperLeft axisPoints.intersect
            let path = pb.Build()
            let drawFunc ctx = draw settings.GridLineStyle.Color settings.GridLineStyle.Thickness path ctx
            drawFunc

        let internal drawMinorGridLines settings minorGridLines ctx =
            minorGridLines
            |> List.iter (fun line ->
                let pb = PathBuilder()
                pb.AddLine(line.start.ToPointF, line.endd.ToPointF) |> ignore
                draw settings.MinorGridLineStyle.Color settings.MinorGridLineStyle.Thickness <| pb.Build() <| ctx)

        let internal drawTitle settings ctx =
            let pb = PathBuilder()
            let pStart, pEnd = calculateTitleLocation settings
            pb.AddLine(pStart, pEnd) |> ignore
            let path = pb.Build()
            drawText settings.Title settings.Font Rgba32.Black path ctx

        let drawLegend (seriesList:TimeSeries<'T> list) settings axisPoints ctx =
            let font = SystemFonts.CreateFont(settings.Font.Name, settings.Font.Size / 2.f, FontStyle.Regular)

            seriesList
            |> List.fold(fun lineX series ->
                let size = getSize font series.title
                let pb2 = PathBuilder()
                let p3 = PointF(lineX, float32 axisPoints.lowerRight.scaledY + size.Height)
                let p4 = PointF(lineX + 20.f, float32 axisPoints.lowerRight.scaledY + size.Height)
                pb2 |> addLine p3 p4
                let path2 = pb2.Build()
                draw series.lineStyle.Color series.lineStyle.Thickness path2 ctx
                
                let textX = lineX + 20.f

                let pb = PathBuilder()
                let textWidth = font.Size * float32 series.title.Length
                let p1 = PointF(textX, float32 axisPoints.lowerRight.scaledY + font.Size * 3.f)
                let p2 = PointF(textX + textWidth, float32 axisPoints.lowerRight.scaledY + font.Size * 3.f)
                pb |> addLine p1 p2
                let path = pb.Build()
                drawText series.title font Rgba32.Black path ctx
                textX + textWidth
            ) (float32 axisPoints.upperLeft.scaledX)
            |> ignore


