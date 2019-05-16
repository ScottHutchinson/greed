module Greed

let score dice =
    let sortedDice = dice |> List.sort
    let calcTripleScore n =
        match n with
        | 1 -> 1000
        | n -> n * 100

    let calcSingleScore n =
        match n with
        | 1 -> 100
        | 5 -> 50
        | _ -> 0

    (* Returns a pair of the maximum scoring triple or 0 if no triples,
       and an index of that triple or None.
    *)
    let tryFindHighestScoringTriple () =
        sortedDice
        |> List.windowed 3
        |> List.mapi (
            fun i w -> if w.[0] = w.[1] && w.[1] = w.[2]
                         then calcTripleScore w.[0], Some i else 0, None)
        |> List.maxBy (fun (s, _) -> s)

    let tripleScore, windowStart = tryFindHighestScoringTriple ()

    let singles windowStart =
        match windowStart with
        | Some 0 -> [sortedDice.[3]; sortedDice.[4]]
        | Some 1 -> [sortedDice.[0]; sortedDice.[4]]
        | Some 2 -> [sortedDice.[0]; sortedDice.[1]]
        | _ -> sortedDice

    let singlesList = singles windowStart
    let singlesScore = singlesList |> List.sumBy calcSingleScore
    tripleScore + singlesScore
