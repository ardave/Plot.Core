namespace Plot.Core

    // TODO:  Maybe make X value a type that is implicitly convertible
    // to float32, based on the constraints of the float32 function
    [<AutoOpen>]
    type public OriginalPoint<'T> = {
        // Original value for UI/Rendering/Display purposes
        originalX : 'T
        // the actual value used for mathematical comparisons
        // and chart rendering/scaling purposes.
        x : float
        y : float
    }