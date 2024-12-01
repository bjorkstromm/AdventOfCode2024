let getLists (str : string[]) =
    str
    |> Array.map (fun x ->
        let nums =
            x.Split(' ', 2, System.StringSplitOptions.TrimEntries)
            |> Array.map int
        (nums.[0], nums.[1]))

let distance (x, y) =
    abs(x - y)

let sort lists =
    lists
    |> Array.unzip
    |> fun (x, y) -> (Array.sort x, Array.sort y)
    ||> Array.zip

let parseFile path =
    path
    |> System.IO.File.ReadAllLines
    |> getLists

let part1 filename =
    filename
    |> parseFile
    |> sort
    |> Array.map distance
    |> Array.sum

// Part 2

let similarityScore xmap x =
    match xmap |> Map.tryFind x with
    | Some c -> (uint64)c * (uint64)x
    | None -> 0UL

let part2 filename =
    let (xs, ys) =
        filename
        |> parseFile
        |> Array.unzip

    let xmap =
        ys
        |> Array.countBy id
        |> Map.ofArray

    xs
    |> Array.map (similarityScore xmap)
    |> Array.sum
