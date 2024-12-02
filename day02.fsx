type Direction =
    | Increasing
    | Decreasing
    | Invalid

let getReports (str : seq<string>) =
    str
    |> Seq.map (fun x ->
        x.Split(' ', System.StringSplitOptions.TrimEntries)
        |> Array.map int
        |> List.ofArray)

let parseFile path =
    path
    |> System.IO.File.ReadLines
    |> getReports

let isSafe report =
    let direction (x, y) =
        match x - y with
        | 1 | 2 | 3 -> Decreasing
        | -1 | -2 | -3 -> Increasing
        | _ -> Invalid

    let isSafe' pairs =
        let dirs = pairs |> Seq.map direction |> Seq.toList
        let res = dirs |> List.forall (fun x -> x = Increasing) || dirs |> List.forall (fun x -> x = Decreasing)
        if res then Some report else None

    report
    |> Seq.windowed 2
    |> Seq.map (fun x -> (x.[0], x.[1]))
    |> isSafe'


let part1 filename =
    filename
    |> parseFile
    |> Seq.choose isSafe
    |> Seq.length


// Part 2

let isSafe2 report =
    let lst = report |> List.ofSeq
    let safe =
        lst
        |> List.ofSeq
        |> List.mapi (fun i _ ->
            lst
            |> List.indexed
            |> List.filter (fun (idx, _) -> idx <> i)
            |> List.map snd
            |> isSafe)
        |> List.choose id
        |> List.length

    if safe > 0 then Some report else None

let part2 filename =
    filename
    |> parseFile
    |> Seq.choose isSafe2
    |> Seq.length