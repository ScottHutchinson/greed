module Greed

let score dice =
    let tripleSize = 3
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
        |> List.windowed tripleSize
        |> List.mapi (
            fun i w -> if w |> List.forall ((=) w.Head)
                         then calcTripleScore w.Head, Some i else 0, None)
        |> List.maxBy (fun (s, _) -> s)

    let tripleScore, windowStart = tryFindHighestScoringTriple ()

    let singles windowStart =
        match windowStart with
        | Some i ->
            (sortedDice |> List.take i) @ 
            (sortedDice |> List.skip (tripleSize - i) |> List.take (tripleSize - 1 - i))
        | _ -> sortedDice

    let singlesList = singles windowStart
    let singlesScore = singlesList |> List.sumBy calcSingleScore
    tripleScore + singlesScore
