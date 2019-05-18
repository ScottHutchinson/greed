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

    let groups = dice |> List.groupBy id

    let tripleScore =
        (0, groups)
        ||> List.fold (fun acc (n, g) ->
                    let len = List.length g
                    let score = if len >= tripleSize then calcTripleScore n else 0
                    max acc score
            )

    let singleScore =
        groups
        |> List.sumBy (fun (n, g) ->
                    let len = List.length g
                    let numSingles = if len >= tripleSize then len - tripleSize else len
                    numSingles * calcSingleScore n
            )

    tripleScore + singleScore
