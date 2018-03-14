namespace Plot.Core

module Settings =
    open SixLabors.ImageSharp
    open SixLabors.Fonts

    type RenderingStyle =
    | Points
    | Lines

    type LineStyle = {
        Color     : Rgba32
        Thickness : float32
    }

    type LineChartSettings = {
        Title               : string
        Width               : int
        Height              : int
        Font                : SixLabors.Fonts.Font
        RenderingStyle      : RenderingStyle
        GridLineStyle       : LineStyle
        DataLineStyle       : LineStyle
        MinorGridLineStyle  : LineStyle
        HorizontalGridLines : int option
        VerticalGridLines   : int option
    }

    let internal calculateFontSize chartWidth =
        let calculatedFontSize = float32 chartWidth / 60.f
        let minReadableFontSize = 12.f
        if calculatedFontSize > minReadableFontSize then calculatedFontSize else minReadableFontSize

    let createLineChartSettings chartTitle width height =
        {
            Title               = chartTitle
            Width               = width
            Height              = height
            Font                = SystemFonts.CreateFont("Arial", calculateFontSize width, FontStyle.Regular)
            RenderingStyle      = Lines
            GridLineStyle       = { Color = Rgba32.Black;     Thickness = 3.f }
            DataLineStyle       = { Color = Rgba32.Orange;    Thickness = 2.f }
            MinorGridLineStyle  = { Color = Rgba32.LightGray; Thickness = 1.f }
            HorizontalGridLines = Some 5
            VerticalGridLines   = Some 10
        }