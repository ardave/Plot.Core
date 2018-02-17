module Foo
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<EntryPoint>]
let main argv = 0