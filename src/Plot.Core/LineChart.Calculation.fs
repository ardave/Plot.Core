namespace Plot.Core.LineChart
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

        let minMaxesCreate minX maxX minY maxY = 
            {
                minX = minMaxCreate minX
                maxX = minMaxCreate maxX
                minY = minY
                maxY = maxY
            }

        let private pointToMinMax point = { originalValue = point.originalX; value = point.x }

        let internal getMinMaxes (firstPoint:OriginalPoint<'T>) points =
            let initialState =
                {
                    minX = { value = firstPoint.x; originalValue = firstPoint.originalX }
                    maxX = { value = firstPoint.x; originalValue = firstPoint.originalX }
                    minY = firstPoint.y
                    maxY = firstPoint.y
                }

            points
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
            upperLeft   : OriginalPoint<'U>
            lowerRight  : OriginalPoint<'U>
            pointWidth  : float
            pointHeight : float
        } 

        let createScalingFactors minMaxes upperLeft lowerRight = 
                {
                    minMaxes = minMaxes
                    chartWidth = lowerRight.x - upperLeft.x
                    chartHeight = lowerRight.y - upperLeft.y
                    upperLeft = upperLeft
                    lowerRight = lowerRight
                    pointWidth = minMaxes.maxX.value - minMaxes.minX.value
                    pointHeight = minMaxes.maxY - minMaxes.minY
                }

        let internal calculateScalingFactors upperLeft lowerRight minMaxes =
            createScalingFactors minMaxes upperLeft lowerRight

        let internal scaleOneYCoordinate sf y =
            let pctH = (sf.minMaxes.maxY - y) / sf.pointHeight
            pctH * sf.chartHeight + sf.upperLeft.y

        let internal scalePointToGrid sf p =
            let pctW = (p.x - sf.minMaxes.minX.value) / sf.pointWidth
            let pctH = (sf.minMaxes.maxY - p.y) / sf.pointHeight
            let scaledPoint = 
                {
                    scaledX = pctW * sf.chartWidth + sf.upperLeft.x
                    scaledY = pctH * sf.chartHeight + sf.upperLeft.y
                }
            scaledPoint

        let internal scalePointsToGrid upperLeft lowerRight firstPoint series =
            let minMaxes = getMinMaxes firstPoint series.originalPoints
            let scalingFactors = calculateScalingFactors upperLeft lowerRight minMaxes
            let scaledSeries = {
                scaledPoints = series.originalPoints |> Array.map (scalePointToGrid scalingFactors)
                lineStyle    = series.lineStyle
                title        = series.title
            }

            scaledSeries, minMaxes, scalingFactors

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
                let endd   = { x = x; y = sf.minMaxes.maxY; originalX = x } |> scalePointToGrid sf
                { 
                    direction = Horizontal
                    label     = (x.ToString())
                    start     = start
                    endd      = endd
                }
            )

        let internal calculateTitleLocation settings =
            let starting = float32 settings.Width / 2.f
            let y = (float32 settings.Height * 0.1f) - (settings.Font.Size * 0.5f)
            let titleWidth = float32 settings.Title.Length * settings.Font.Size
            let pStart = PointF(starting - titleWidth * 0.25f, y)
            let pEnd   = PointF(starting + titleWidth * 0.75f, y)
            pStart, pEnd
