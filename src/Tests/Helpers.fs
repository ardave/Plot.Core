module Helpers

open System

let shouldEqual expected actual =
    if actual <> expected then
        failwithf "Expected %A but was %A!" expected actual

let shouldEqualWithin tolerance (expected:float) actual = 
    let difference = Math.Abs(actual - expected)
    if difference > tolerance then
        failwithf "Expected %f and %f to be equal within %f, but differed by %f" expected actual tolerance difference