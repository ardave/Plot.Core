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

        let internal scaleOneYCoordinate sf y =
            let pctH = (sf.minMaxes.maxY - y) / sf.PointHeight
            pctH * sf.chartHeight + sf.upperLeft.y

        let internal scalePointToGrid sf p =
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

        let internal calcMinorGridLineIncrement (scalingFactors:ScalingFactors<'T, 'U>) numGridLines =
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
            scalingFactors.PointHeight / float numGridLines

        let internal calcMinorGridLinesPoints numLines sf increment =
            [0.. numLines]
            |> List.map(fun n ->
                // let y = (lowerRight.y - increment * float n)
                let y = sf.minMaxes.minY + increment * float n
                printfn "*********************************"
                printfn "minY = %f, increment = %f, n = %i" sf.minMaxes.minY increment n
                // printfn "y = %f" y
                let newStart = { x = sf.minMaxes.minX.value; y = y; originalX = sf.minMaxes.minX.originalValue } |> scalePointToGrid sf
                let newEnd   = { x = sf.minMaxes.maxX.value; y = y; originalX = sf.minMaxes.maxX.originalValue } |> scalePointToGrid sf
                printfn "newStart = %A\nnewEnd = %A" newStart newEnd
                newStart, newEnd
            )
        let internal calculateTitleLocation settings =
            let starting = float32 settings.Width / 2.f
            let y = float32 settings.Height * 0.1f
            let titleWidth = float32 settings.Title.Length * settings.Font.Size
            let pStart = PointF(starting - titleWidth * 0.25f, y)
            let pEnd   = PointF(starting + titleWidth * 0.75f, y)
            pStart, pEnd
