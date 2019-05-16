module GreedTest

open FsUnit.Xunit
open Xunit
open Greed

[<Fact>]
let ``Gets score for triple aces (at the beginning)`` () =
    let digits = [1;1;1;5;1]
    score digits |> should equal 1150

[<Fact>]
let ``Gets score for no points`` () =
    let digits = [2;3;4;6;2]
    score digits |> should equal 0

[<Fact>]
let ``Gets score for one triple`` () =
    let digits = [3;4;5;3;3]
    score digits |> should equal 350

[<Fact>]
let ``Gets score for no triples and multiple singles`` () =
    let digits = [1;5;1;2;4]
    score digits |> should equal 250

[<Fact>]
let ``Gets score for multiple triples`` () =
    let digits = [5;5;5;5;5]
    score digits |> should equal 600

[<Fact>]
let ``Gets score for triple in the middle`` () =
    let digits = [2;2;2;1;4]
    score digits |> should equal 300

[<Fact>]
let ``Gets score for triple at the end`` () =
    let digits = [2;2;2;1;1]
    score digits |> should equal 400
