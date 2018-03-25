namespace Plot.Core.LineChart
    open SixLabors.Fonts

    [<AutoOpen>]
    module internal Calculation =
        open System
        open SixLabors.Primitives
        open Plot.Core
        open Plot.Core.Settings

        type internal MinMax<'T> = {
            value : float
            originalValue : 'T
        }
        
        let minMaxCreate v = { value = v; originalValue = v }
        
        type internal MinMaxes<'T> = {
            minX : MinMax<'T>
            maxX : MinMax<'T>
            minY : float
            maxY : float
        }

        type internal AxisPoints = {
            upperLeft  : ScaledPoint
            intersect  : ScaledPoint
            lowerRight : ScaledPoint
        } 
        with static member Create ul lr = 
                {
                    upperLeft = ul
                    lowerRight = lr
                    intersect = { scaledX = ul.scaledX; scaledY = lr.scaledY }
                }
        let minMaxesCreate minX maxX minY maxY = 
            {
                minX = minMaxCreate minX
                maxX = minMaxCreate maxX
                minY = minY
                maxY = maxY
            }

        let private pointToMinMax point = { originalValue = point.originalX; value = point.x }

        let private getSize font text = TextMeasurer.Measure(text, new RendererOptions(font))

        let internal getMinMaxes firstPoint seriesList =
            let initialState =
                {
                    minX = { value = firstPoint.x; originalValue = firstPoint.originalX }
                    maxX = { value = firstPoint.x; originalValue = firstPoint.originalX }
                    minY = firstPoint.y
                    maxY = firstPoint.y
                }

            seriesList
            |> List.map(fun x -> x.originalPoints)
            |> List.toArray
            |> Array.collect id
            |> Array.fold (fun minMaxes point ->
                let minMax = {
                    minX = if point.x < minMaxes.minX.value then pointToMinMax point else minMaxes.minX
                    minY = Math.Min(minMaxes.minY, point.y)
                    maxX = if point.x > minMaxes.maxX.value then pointToMinMax point else minMaxes.maxX
                    maxY = Math.Max(minMaxes.maxY, point.y)
                }
                minMax
                ) initialState

        type ScalingFactors<'T, 'U> = {
            minMaxes    : MinMaxes<'T>
            chartWidth  : float
            chartHeight : float
            axisPoints  : AxisPoints
            pointWidth  : float
            pointHeight : float
        } 

        let createScalingFactors minMaxes axisPoints = 
                {
                    minMaxes    = minMaxes
                    chartWidth  = axisPoints.lowerRight.scaledX - axisPoints.upperLeft.scaledX
                    chartHeight = axisPoints.lowerRight.scaledY - axisPoints.upperLeft.scaledY
                    axisPoints  = axisPoints 
                    pointWidth  = minMaxes.maxX.value - minMaxes.minX.value
                    pointHeight = minMaxes.maxY - minMaxes.minY
                }

        let internal calculateScalingFactors axisPoints minMaxes =
            createScalingFactors minMaxes axisPoints

        let internal scaleOneYCoordinate sf y =
            let pctH = (sf.minMaxes.maxY - y) / sf.pointHeight
            pctH * sf.chartHeight + sf.axisPoints.upperLeft.scaledY

        let internal scalePointToGrid sf p =
            let pctW = (p.x - sf.minMaxes.minX.value) / sf.pointWidth
            let pctH = (sf.minMaxes.maxY - p.y) / sf.pointHeight
            let scaledPoint = 
                {
                    scaledX = pctW * sf.chartWidth + sf.axisPoints.upperLeft.scaledX
                    scaledY = pctH * sf.chartHeight + sf.axisPoints.upperLeft.scaledY
                }
            scaledPoint

        let internal scalePointsToGrid axisPoints firstPoint seriesList =
            let minMaxes = getMinMaxes firstPoint seriesList
            let scalingFactors = calculateScalingFactors axisPoints minMaxes

            let scaledSeriesList =
                seriesList
                |> List.map(fun series -> 
                    {
                        scaledPoints = series.originalPoints |> Array.map (scalePointToGrid scalingFactors)
                        lineStyle    = series.lineStyle
                        title        = series.title
                    })

            scaledSeriesList, minMaxes, scalingFactors

        let internal calcMinorGridLineIncrement span numGridLines =
            // let rec getIncrement input multiplier =
            //     printfn "Input = %f" input
            //     match input with
            //     | i when i > 1. ->
            //         getIncrement (i / 10.) (multiplier * 10.)
            //     | i when i <= 1. ->
            //         getIncrement (i * 10.) (multiplier / 10.)
            //     | _ -> Math.Ceiling input * multiplier
            // printfn "PointHeight = %f, numGridLines = %i" scalingFactors.PointHeight numGridLines
            // getIncrement (scalingFactors.PointHeight / float numGridLines) 1.
            span / float numGridLines

        let internal soStrategy span numGridLines =
            let minimum = span / float numGridLines
            let magnitude = 10. ** Math.Floor(Math.Log(minimum) / Math.Log(10.))
            let residual = minimum / magnitude
            let result = 
                match residual with
                | x when x > 5. -> 10. * magnitude
                | x when x > 2. -> 5.  * magnitude
                | x when x > 1. -> 2.  * magnitude
                | _ -> magnitude
            result

        let internal calcMinorHorizontalGridLinesPoints numLines sf increment =
            [0.. numLines]
            |> List.map(fun n ->
                let y = sf.minMaxes.minY + increment * float n
                let start = { x = sf.minMaxes.minX.value; y = y; originalX = sf.minMaxes.minX.originalValue } |> scalePointToGrid sf
                let endd   = { x = sf.minMaxes.maxX.value; y = y; originalX = sf.minMaxes.maxX.originalValue } |> scalePointToGrid sf
                { 
                    direction = Horizontal
                    label     = (y.ToString())
                    start     = start
                    endd      = endd
                }
            )

        let internal calcMinorVerticalGridLinesPoints numLines sf increment =
            [0.. numLines]
            |> List.map(fun n ->
                let x = sf.minMaxes.minX.value + increment * float n
                let start = { x = x; y = sf.minMaxes.minY; originalX = x } |> scalePointToGrid sf
                let endd  = { x = x; y = sf.minMaxes.maxY; originalX = x } |> scalePointToGrid sf
                { 
                    direction = Horizontal
                    label     = (x.ToString())
                    start     = start
                    endd      = endd
                }
            )

        let internal getMinorGridLinePoints scalingFactors numLines =
            let horizontalLines =
                calcMinorGridLineIncrement scalingFactors.pointHeight numLines
                |> calcMinorHorizontalGridLinesPoints numLines scalingFactors

            let verticalLines =
                calcMinorGridLineIncrement scalingFactors.pointWidth numLines
                |> calcMinorVerticalGridLinesPoints numLines scalingFactors

            horizontalLines @ verticalLines

        let internal calculateTitleLocation settings =
            let starting = float32 settings.Width / 2.f
            let y = (float32 settings.Height * 0.1f) - (settings.Font.Size * 0.5f)
            let titleWidth = float32 settings.Title.Length * settings.Font.Size
            let pStart = PointF(starting - titleWidth * 0.25f, y)
            let pEnd   = PointF(starting + titleWidth * 0.75f, y)
            pStart, pEnd

        let internal calculateAxisPoints settings =
            let w = float settings.Width
            let h = float settings.Height
            let proportion = 0.9 // proportion of image used for the grid
            let upperLeft  = { scaledX = w * (1. - proportion); scaledY = h * (1. - proportion) }
            let lowerRight = { scaledX = w * proportion;        scaledY = h * proportion }
            AxisPoints.Create upperLeft lowerRight

        let getFontSize settings minMaxes verticalSpace =
            let minXStr = minMaxes.minX.originalValue.ToString()
            [|0..30|]
            |> Array.map float32
            |> Array.filter(fun x ->
                let font = SystemFonts.CreateFont(settings.Font.Name, float32 x, FontStyle.Regular)
                float (getSize font minXStr).Height < verticalSpace )
            |> Array.max

        let calcMinXPosition minMaxes axisPoints settings fontSize =
            let minXStr = minMaxes.minX.originalValue.ToString()
            let font = SystemFonts.CreateFont(settings.Font.Name, fontSize, FontStyle.Regular)
            let size = getSize font minXStr
            let startPoint = { scaledX = axisPoints.upperLeft.scaledX;  scaledY = axisPoints.lowerRight.scaledY + float size.Height / 2. }
            let endPoint   = { scaledX = axisPoints.lowerRight.scaledX; scaledY = axisPoints.lowerRight.scaledY + float size.Height / 2. }

            startPoint, endPoint

        let calcMaxXPosition minMaxes axisPoints settings fontSize =
            let maxXStr = minMaxes.maxX.originalValue.ToString()
            let font = SystemFonts.CreateFont(settings.Font.Name, fontSize, FontStyle.Regular)
            let size = getSize font maxXStr
            let startPoint = { scaledX = axisPoints.lowerRight.scaledX - float size.Width; scaledY = axisPoints.lowerRight.scaledY + float size.Height / 2. }
            let endPoint   = { scaledX = axisPoints.lowerRight.scaledX + float size.Width; scaledY = axisPoints.lowerRight.scaledY + float size.Height / 2. }
            startPoint, endPoint

        let internal calcMinYPosition minMaxes axisPoints settings =
            let spacing    = 3.
            let minYStr    = minMaxes.minY.ToString()
            let size       = getSize settings.Font minYStr
            let startx     = axisPoints.upperLeft.scaledX - (float size.Width + spacing)
            let endx       = axisPoints.lowerRight.scaledX
            let y          = axisPoints.lowerRight.scaledY - float size.Height / 2.
            let startPoint = { scaledX = startx; scaledY = y }
            let endPoint   = { scaledX = endx;   scaledY = y}
            startPoint, endPoint

        let internal calcMaxYPosition minMaxes axisPoints settings =
            let spacing    = 3.
            let maxYStr    = minMaxes.maxY.ToString()
            let size       = getSize settings.Font maxYStr
            let startX     = axisPoints.upperLeft.scaledX - (float size.Width + spacing)
            let endX       = axisPoints.upperLeft.scaledX
            let y          = axisPoints.upperLeft.scaledY + (float size.Height / 2.)
            let startPoint = { scaledX = startX; scaledY =  y }
            let endPoint   = { scaledX = endX; scaledY = y }
            startPoint, endPoint

        let calculateLegend fontSize xAxisLabelsVerticalSpace legendVerticalSpace axisPoints settings (seriesList:TimeSeries<'a> list) =
            let font = SystemFonts.CreateFont(settings.Font.Name, fontSize, FontStyle.Regular)

            let startX = axisPoints.intersect.scaledX
            let startY = axisPoints.intersect.scaledY + xAxisLabelsVerticalSpace + legendVerticalSpace / 2.

            let mapping (startX, y) (ts:TimeSeries<'a>) =
                let size = getSize font ts.title
                let lineSampleWidth = float settings.Width / 50.
                let lineStartX = startX
                let lineEndX = lineStartX + lineSampleWidth
                let textStartX = lineEndX + 2.
                let textEndX = textStartX + float size.Width + float settings.Width / 100.
                
                let legendEntry =
                    {
                        title             = ts.title
                        lineStartPosition = { scaledX = lineStartX; scaledY = y }
                        lineEndPosition   = { scaledX = lineEndX;   scaledY = y }
                        textStartPosition = { scaledX = textStartX ; scaledY = y }
                        textEndPosition   = { scaledX = textEndX;   scaledY = y }
                        lineStyle         = ts.lineStyle
                    }

                legendEntry, (textEndX, y)
            let state = startX, startY
            let lst, _ = List.mapFold mapping state seriesList

            let legend =
                {
                    fontSize = fontSize
                    entries  = lst
                }
            legend