namespace Plot.Core.LineChart
    module internal Calculation =
        open System
        open Plot.Core

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
                    minX = 
                        {
                            value = firstPoint.x
                            originalValue = firstPoint.originalX
                        }
                    maxX =
                        {
                            value = firstPoint.x
                            originalValue = firstPoint.originalX
                        }
                    minY = firstPoint.y
                    maxY = firstPoint.y
                }

            points
            |> Array.fold (fun minMaxes point ->
                {
                    minX = if point.x < minMaxes.minX.value then pointToMinMax point else minMaxes.minX
                    minY = Math.Min(minMaxes.minY, point.y)
                    maxX = if point.x > minMaxes.maxX.value then pointToMinMax point else minMaxes.maxX
                    maxY = Math.Max(minMaxes.maxY, point.y)
                }) initialState