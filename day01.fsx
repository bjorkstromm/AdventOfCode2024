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