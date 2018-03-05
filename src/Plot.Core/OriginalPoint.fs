namespace Plot.Core
    [<AutoOpen>]
    module OriginalPoint =
        open SixLabors.Primitives

        type public OriginalPoint<'T> = {
            // Original value for UI/Rendering/Display purposes
            originalX : 'T
            // Original value provided in the user's units
            x : float
            y : float
        }

        let create x y = { x = x; y = y; originalX = x }

        // Data point scaled to fit the grid lines in the chart
        type internal ScaledPoint ={
            scaledX : float
            scaledY : float
        } with member this.ToPointF = PointF(float32 this.scaledX, float32 this.scaledY)