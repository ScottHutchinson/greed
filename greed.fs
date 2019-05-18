module Greed

let score dice =
    let tripleSize = 3

    let calcTripleScore n =
        match n with
        | 1 -> 1000
        | n -> n * 100

    let calcSingleScore n =
        match n with
        | 1 -> 100
        | 5 -> 50
        | _ -> 0

    dice
    |> List.groupBy id
    |> List.sumBy (fun (n, g) ->
            let len = List.length g
            let numSingles = if len >= tripleSize then len - tripleSize else len
            let tripleScore = if len >= tripleSize then calcTripleScore n else 0
            tripleScore + numSingles * calcSingleScore n
        )
