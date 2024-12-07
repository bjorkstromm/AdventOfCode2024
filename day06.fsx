type Direction = North | East | South | West

let locateStart map =
    let rec loop (y, x) =
        if y >= Array2D.length1 map then None
        elif x >= Array2D.length2 map then loop ((y + 1), 0)
        elif map.[y, x] = '^' then Some (y, x)
        else loop (y, (x + 1))

    let (y, x) = loop (0, 0) |> Option.get

    let newMap = Array2D.copy map
    newMap.[y, x] <- '.'

    ((y, x), North, newMap)

let parseMap filename =
    filename
    |> System.IO.File.ReadAllLines
    |> array2D
    |> locateStart

let move ((y, x), direction, map) =
    let yMax = Array2D.length1 map
    let xMax = Array2D.length2 map

    let rec loop visited (y, x) direction =
        let (dy, dx) =
            match direction with
            | North -> (-1, 0)
            | East -> (0, 1)
            | South -> (1, 0)
            | West -> (0, -1)

        let (y', x') = (y + dy, x + dx)

        if y' < 0 || y' >= yMax || x' < 0 || x' >= xMax then visited |> List.rev
        elif map.[y', x'] = '#' then
            let direction' =
                match direction with
                | North -> East
                | East -> South
                | South -> West
                | West -> North

            loop visited (y, x) direction'
        else
            let visited' = (y', x') :: visited
            loop visited' (y', x') direction

    loop [(y, x)] (y,x) direction


let part1 filename =
    filename
    |> parseMap
    |> move
    |> List.distinct
    |> List.length