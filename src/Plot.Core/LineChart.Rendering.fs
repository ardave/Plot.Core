namespace Plot.Core.LineChart

    module internal Rendering =

        open SixLabors.Primitives
        open SixLabors.Shapes
        open Plot.Core
        open LineChart.Calculation

        type internal FittedPoint ={
            fittedX : float
            fittedY : float
        } with member this.ToPointF = PointF(float32 this.fittedX, float32 this.fittedY)

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