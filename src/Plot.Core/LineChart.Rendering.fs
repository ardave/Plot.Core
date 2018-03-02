namespace Plot.Core.LineChart
    module internal Rendering =
        open SixLabors.Primitives
        open Plot.Core
        open LineChart.Calculation

        let originalToPointF o = PointF(float32 o.x, float32 o.y)

        type internal FittedPoint ={
            fittedX : float
            fittedY : float
        } with member this.ToPointF = PointF(float32 this.fittedX, float32 this.fittedY)