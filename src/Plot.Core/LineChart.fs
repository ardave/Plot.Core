namespace Plot.Core.LineChart

[<AutoOpen>]
module LineChart =

    open Plot.Core
    open Plot.Core.LineChart

    let createLineChart settings seriesList =
        let axisPoints = calculateAxisPoints settings

        match seriesList |> List.tryHead with
        | None             -> None
        | Some firstSeries ->
            match firstSeries.originalPoints |> Array.tryGet 0 with
            | None            -> None
            | Some firstPoint -> 
                let scaledSeriesList, minMaxes, scalingFactors = scalePointsToGrid axisPoints firstPoint seriesList
                let minXPosition = calcMinXPosition minMaxes axisPoints settings.Font
                let maxXPosition = calcMaxXPosition minMaxes settings.Font axisPoints.lowerRight
                let minYPosition = calcMinYPosition minMaxes axisPoints settings.Font
                let maxYPosition = calcMaxYPosition minMaxes axisPoints.upperLeft settings.Font
                
                let img = drawLineChart axisPoints settings seriesList scaledSeriesList scalingFactors minMaxes minXPosition minYPosition maxXPosition maxYPosition
                Some img
