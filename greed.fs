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

    let rec calc acc lst =
        match lst with
        | [] -> acc
        | a :: b :: c :: t when a = b && b = c ->
            calc (acc + calcTripleScore a) t
        | h :: t ->
            calc (acc + calcSingleScore h) t

    calc 0 sortedDice
