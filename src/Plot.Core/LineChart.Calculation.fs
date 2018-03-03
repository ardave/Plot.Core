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
        
        type internal MinMaxes<'T> = {
            minX : MinMax<'T>
            maxX : MinMax<'T>
            minY : float
            maxY : float
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
        } with
            member x.PointWidth  = x.minMaxes.maxX.value - x.minMaxes.minX.value
            member x.PointHeight = x.minMaxes.maxY - x.minMaxes.minY

        let internal calculateScalingFactors upperLeft lowerRight minMaxes =
            {
                minMaxes    = minMaxes
                chartWidth  = lowerRight.x - upperLeft.x
                chartHeight = lowerRight.y - upperLeft.y
                lowerRight  = lowerRight
                upperLeft   = upperLeft
            }

        let inline internal scaleOneYCoordinate sf y =
            let pctH = (sf.minMaxes.maxY - y) / sf.PointHeight
            pctH * sf.chartHeight + sf.upperLeft.y

        let inline internal scalePointToGrid sf p =
            let pctW = (p.x - sf.minMaxes.minX.value) / sf.PointWidth
            let pctH = (sf.minMaxes.maxY - p.y) / sf.PointHeight
            let scaledPoint = 
                {
                    scaledX = pctW * sf.chartWidth + sf.upperLeft.x
                    scaledY = pctH * sf.chartHeight + sf.upperLeft.y
                }
            scaledPoint

        let internal scalePointsToGrid upperLeft lowerRight firstPoint points =
            let minMaxes = getMinMaxes firstPoint points
            let scalingFactors = calculateScalingFactors upperLeft lowerRight minMaxes
            let scaledPoints = points |> Array.map (scalePointToGrid scalingFactors)
            scaledPoints, minMaxes, scalingFactors

        let internal calcMinorGridLineIncrement maxValue numGridLines =
            let rec getIncrement input multiplier =
                match input with
                | i when i > 10. ->
                    getIncrement (i / 10.) (multiplier * 10.)
                | i when i <= 1. ->
                    getIncrement (i * 10.) (multiplier / 10.)
                | _ -> Math.Ceiling(float input) * multiplier
            getIncrement (maxValue / float numGridLines) 1.

        let internal calcMinorGridLinesPoints upperLeft lowerRight numLines increment sf =
            [0.. numLines]
            |> List.map(fun n ->
                let x1 = upperLeft.x
                let y1 = lowerRight.y - increment * float n |> scaleOneYCoordinate sf
                let x2 = lowerRight.x
                let y2 = y1
                let newStart = { x = x1; y = y1; originalX = x1 }
                let newEnd   = { x = x2; y = y2; originalX = x2 }
                newStart, newEnd
            )
        let internal calculateTitleLocation settings =
            let starting = float32 settings.Width / 2.f
            let y = float32 settings.Height * 0.1f
            let titleWidth = float32 settings.Title.Length * settings.Font.Size
            let pStart = PointF(starting - titleWidth * 0.25f, y)
            let pEnd   = PointF(starting + titleWidth * 0.75f, y)
            pStart, pEnd
