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
                let totalVerticalSpace    = float settings.Height - axisPoints.intersect.scaledY

                let yAxisLabelsHorizSpace = axisPoints.intersect.scaledX
                let minYLabelFontSize = getFontSize settings (minMaxes.minY.ToString()) yAxisLabelsHorizSpace
                let maxYLabelFontSize = getFontSize settings (minMaxes.maxY.ToString()) yAxisLabelsHorizSpace

                printfn "yAxisLabelsHorizSpace %f" yAxisLabelsHorizSpace
                printfn "minYLabelFontSize %f" minYLabelFontSize
                printfn "maxYLabelFontSize %f" maxYLabelFontSize
                                
                let xAxisLabelsVerticalSpace = totalVerticalSpace * 0.6
                let xAxisLabelsFontSize = (getFontSize settings (minMaxes.minX.originalValue.ToString()) xAxisLabelsVerticalSpace) - 7.5f

                let legendVerticalSpace = totalVerticalSpace * 0.4
                let legendFontSize = getFontSize settings (minMaxes.minX.originalValue.ToString()) legendVerticalSpace

                let legend = calculateLegend legendFontSize xAxisLabelsVerticalSpace legendVerticalSpace axisPoints settings seriesList
                let minXPosition = calcMinXPosition minMaxes axisPoints settings xAxisLabelsFontSize
                let maxXPosition = calcMaxXPosition minMaxes axisPoints settings xAxisLabelsFontSize
                let minYPosition = calcMinYPosition minMaxes axisPoints settings
                let maxYPosition = calcMaxYPosition minMaxes axisPoints settings
                
                let img = drawLineChart axisPoints settings scaledSeriesList scalingFactors minMaxes minXPosition minYPosition maxXPosition maxYPosition xAxisLabelsFontSize legend minYLabelFontSize maxYLabelFontSize
                Some img
