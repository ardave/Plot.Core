namespace Plot.Core.LineChart
    module internal Calculation =

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