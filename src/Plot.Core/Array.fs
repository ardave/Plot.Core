namespace Plot.Core

module Array =
    let tryGet idx ary =
        if (ary |> Array.length) - 1 > idx then Some ary.[idx] else None