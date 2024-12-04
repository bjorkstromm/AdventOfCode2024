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