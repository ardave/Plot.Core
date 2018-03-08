module Helpers

let shouldEqual expected actual =
    if actual <> expected then
        failwithf "Expected %A but was %A!" expected actual