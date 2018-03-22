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

        let internal drawMaxY minMaxes maxYPosition font ctx =
            let maxYStr = minMaxes.maxY.ToString()
            let path = PathBuilder()
                        .AddLine(maxYPosition |> fst |> scaledToPointF, maxYPosition |> snd |> scaledToPointF)
                        .Build()
            drawText maxYStr font Rgba32.Black path ctx

        let internal drawMinY minYPosition minMaxes font =
            let minYStr = minMaxes.minY.ToString()
            let path = PathBuilder()
                        .AddLine(minYPosition |> fst |> scaledToPointF, minYPosition |> snd |> scaledToPointF)
                        .Build()
            let f = drawText minYStr font Rgba32.Black path
            f

        let internal drawMinX minXPosition minMaxes settings xAxisLabelsFontSize =
            let minXStr = minMaxes.minX.originalValue.ToString()
            let path = PathBuilder()
                        .AddLine(minXPosition |> fst |> scaledToPointF, minXPosition |> snd |> scaledToPointF)
                        .Build()
            let font = SystemFonts.CreateFont(settings.Font.Name, xAxisLabelsFontSize, FontStyle.Regular)
            let f = drawText minXStr font Rgba32.Black path
            f

        let internal drawMaxX maxXPosition minMaxes settings xAxisLabelsFontSize =
            let maxXStr = minMaxes.maxX.originalValue.ToString()
            let path = PathBuilder()
                        .AddLine(maxXPosition |> fst |> scaledToPointF, maxXPosition |> snd |> scaledToPointF)
                        .Build()
            let font = SystemFonts.CreateFont(settings.Font.Name, xAxisLabelsFontSize, FontStyle.Regular)
            let f = drawText maxXStr font Rgba32.Black path
            f

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
            pb |> addLineF axisPoints.intersect axisPoints.lowerRight
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
            let pStart, pEnd = calculateTitleLocation settings
            let path = PathBuilder()
                        .AddLine(pStart, pEnd)
                        .Build()
            drawText settings.Title settings.Font Rgba32.Black path ctx

        let drawLegend (seriesList:TimeSeries<'T> list) settings axisPoints fontSize ctx =
            let font = SystemFonts.CreateFont(settings.Font.Name, fontSize, FontStyle.Regular)
            printfn "Legend font size is: %A" fontSize
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
                printfn "Drawing legend for '%s' from (%A, %A) to (%A, %A)" series.title p3 p4 p1 p2
                textX + textWidth

            ) (float32 axisPoints.upperLeft.scaledX)
            |> ignore

        let internal assembleMinorGridLinesFunctions settings scalingFactors =
            match settings.HorizontalGridLines with
            | None -> ignore
            | Some numLines ->
                getMinorGridLinePoints scalingFactors numLines
                |> drawMinorGridLines settings

        let internal drawLineChart axisPoints settings seriesList scaledSeriesList scalingFactors minMaxes minXPosition minYPosition maxXPosition maxYPosition xAxisLabelsFontSize legendFontSize =
            let img = new Image<Rgba32>(settings.Width, settings.Height)
            img.Mutate(fun ctx ->
                fillBackground ctx
                drawMajorGridLines axisPoints settings ctx
                drawTitle settings ctx
                drawLegend seriesList settings axisPoints legendFontSize ctx
                assembleMinorGridLinesFunctions settings scalingFactors ctx
                scaledSeriesList |> List.iter(fun x -> drawDataLines x ctx)
                drawMaxX maxXPosition minMaxes settings xAxisLabelsFontSize ctx
                drawMinX minXPosition minMaxes settings xAxisLabelsFontSize ctx
                drawMinY minYPosition minMaxes settings.Font ctx
                drawMaxY minMaxes maxYPosition settings.Font ctx
                )
            img


