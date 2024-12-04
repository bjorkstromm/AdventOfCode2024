let parseFile filename =
    filename
    |> System.IO.File.ReadAllLines
    |> array2D

let sliceFromIndex arr len (x, y) =
    let rows = Array2D.length1 arr
    let cols = Array2D.length2 arr

    let isValid (x, y) =
        x >= 0  && x < rows 
        && y >= 0 && y < cols

    // Collect elements along a direction using a step for (dx, dy)
    let collect (dx, dy) =
        [| for step in 0 .. len - 1 do
               let i = x + step * dx
               let j = y + step * dy
               if isValid (i, j) then yield arr.[i, j] |]

    // Return all slices
    [|(1,1);(1,-1);(-1,1);(-1,-1);(0,1);(1,0);(0,-1);(-1,0)|]
    |> Array.map collect
    |> Array.filter (fun slice -> slice.Length = len)

let part1 filename =
    let str = "XMAS".ToCharArray()
    let arr = parseFile filename

    seq {
        for x in 0 .. Array2D.length1 arr - 1 do
            for y in 0 .. Array2D.length2 arr - 1 do
                yield (x, y)
    }
    |> Seq.collect (fun (x, y) -> sliceFromIndex arr 4 (x, y))
    |> Seq.filter (fun slice -> slice = str)
    |> Seq.length

// Part 2
let extractSquare (x, y) size (arr : 'a[,]) =
    Array2D.init size size (fun x' y' -> arr.[x + x', y + y'])

let extractSquares size arr =
    seq {
        for x in 0 .. Array2D.length1 arr - size do
            for y in 0 .. Array2D.length2 arr - size do
                yield (x, y)
    }
    |> Seq.map (fun (x, y) -> extractSquare (x, y) size arr)

let extractDiagonals arr =
    let rows = Array2D.length1 arr
    let cols = Array2D.length2 arr
    let size = min rows cols
    [|
        [| for i in 0 .. size - 1 -> arr.[i, i] |]
        [| for i in 0 .. size - 1 -> arr.[i, size - i - 1] |]
    |]

let part2 filename =
    let valid =
        [|"MAS".ToCharArray(); "SAM".ToCharArray() |]
        |> Set.ofArray

    let isValid slice =
        valid |> Set.contains slice

    filename
    |> parseFile
    |> extractSquares 3
    |> Seq.map (fun square ->
        square
        |> extractDiagonals
        |> Seq.filter isValid)
    |> Seq.filter (Seq.length >> (=) 2)
    |> Seq.length