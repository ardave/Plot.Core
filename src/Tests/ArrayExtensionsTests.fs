module ArrayExtensionsTests
open Xunit
open FsUnit.Xunit
open Plot.Core

[<Fact>]
let ``tryGet should return None if array is empty`` () =
    let opt = [||] |> Array.tryGet 0
    opt.IsNone |> should be True

[<Fact>]
let ``tryGet should return Some element if the element's array index is requested``() =
    [| "foo" |] 
    |> Array.tryGet 0
    |> Option.isSome
    |> should be True

    [| "foo" |]
    |> Array.tryGet 1
    |> Option.isNone
    |> should be True

[<EntryPoint>]
let main _ = 0