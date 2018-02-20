namespace Plot.Core

module Settings =
    open System
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
        Title          : string
        Width          : int
        Height         : int
        GridColor      : Rgba32
        DataColor      : Rgba32
        Font           : SixLabors.Fonts.Font
        RenderingStyle : RenderingStyle
        GridLineStyle  : LineStyle
        DataLineStyle  : LineStyle
    }

    let internal calculateFontSize chartWidth =
        let calculatedFontSize = float32 chartWidth / 60.f
        let minReadableFontSize = 12.f
        if calculatedFontSize > minReadableFontSize then calculatedFontSize else minReadableFontSize

    let createLineChartSettings chartTitle width height =
        {
            Title          = chartTitle
            Width          = width
            Height         = height
            GridColor      = Rgba32.Black
            DataColor      = Rgba32.Black
            Font           = SystemFonts.CreateFont("Arial", calculateFontSize width, FontStyle.Regular)
            RenderingStyle = Lines
            GridLineStyle  = { Color = Rgba32.Black;  Thickness = 3.f }
            DataLineStyle  = { Color = Rgba32.Orange; Thickness = 2.f }
        }