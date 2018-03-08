namespace Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Plot.Core
open Helpers

[<TestClass>]
type ArrayExtensionsTests () =
    [<TestMethod>]
    member this.``tryGet should return None if array is empty`` () =
        let opt = [||] |> Array.tryGet 0
        opt.IsNone |> shouldEqual true

    [<TestMethod>]
    member this.``tryGet should return Some element if the element's array index is requested``() =
        [| "foo" |] 
        |> Array.tryGet 0
        |> Option.isSome
        |> shouldEqual true

        [| "foo" |]
        |> Array.tryGet 1
        |> Option.isNone
        |> shouldEqual true
