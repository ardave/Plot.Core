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
                let totalVerticalSpace = (float settings.Height - axisPoints.intersect.scaledY)
                printfn "settings.Height: %A" settings.Height
                printfn "axisPoints.intersect.scaledX: %A" axisPoints.intersect.scaledX
                printfn "totalVerticalSpace: %A" totalVerticalSpace
                let xAxisLabelsVerticalSpace = totalVerticalSpace * 0.6
                let xAxisLabelsFontSize = getFontSize settings minMaxes xAxisLabelsVerticalSpace

                let legendVerticalSpace = totalVerticalSpace * 0.4
                let legendFontSize = getFontSize settings minMaxes legendVerticalSpace

                let legend = calculateLegend legendFontSize xAxisLabelsVerticalSpace legendVerticalSpace axisPoints settings seriesList
                let minXPosition = calcMinXPosition minMaxes axisPoints settings xAxisLabelsFontSize
                let maxXPosition = calcMaxXPosition minMaxes axisPoints settings xAxisLabelsFontSize 
                let minYPosition = calcMinYPosition minMaxes axisPoints settings
                let maxYPosition = calcMaxYPosition minMaxes axisPoints settings
                
                let img = drawLineChart axisPoints settings scaledSeriesList scalingFactors minMaxes minXPosition minYPosition maxXPosition maxYPosition xAxisLabelsFontSize legend
                Some img
