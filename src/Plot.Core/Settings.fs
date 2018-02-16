namespace Plot.Core

module Settings =
    open SixLabors.ImageSharp

    type RenderingStyle =
    | Points
    | Lines

    type LineStyle = {
        Color     : Rgba32
        Thickness : float32
    }

    type LineChartSettings = {
        Title          : string
        Width          : int
        Height         : int
        GridColor      : Rgba32
        DataColor      : Rgba32
        RenderingStyle : RenderingStyle
        GridLineStyle  : LineStyle
        DataLineStyle  : LineStyle
    }

    let createLineChartSettings chartTitle =
        {
            Title          = chartTitle
            Width          = 1500
            Height         = 500
            GridColor      = Rgba32.Black
            DataColor      = Rgba32.Black
            RenderingStyle = Lines
            GridLineStyle  = { Color = Rgba32.Black;  Thickness = 3.f }
            DataLineStyle  = { Color = Rgba32.Orange; Thickness = 2.f }
        }