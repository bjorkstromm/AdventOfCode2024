type Equation = {
    Result: int64
    Numbers: int64 list
}

type Operator =
    | Add
    | Multiply

let parseEquation (line : string) =
    let tokens = line.Split(':', 2, System.StringSplitOptions.RemoveEmptyEntries)

    let result = System.Int64.Parse(tokens.[0])
    let numbers =
        tokens.[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map System.Int64.Parse
        |> Array.toList

    { Result = result; Numbers = numbers }

let parseFile filename =
    System.IO.File.ReadAllLines filename
    |> Array.map parseEquation

let solve equation =
    let rec loop (res, operations) numbers =
        match numbers with
        | [] when res = equation.Result -> [Some (operations |> List.rev)]
        | [] -> [None]
        | x :: tail when res <= equation.Result ->
            let add = loop (res + x, Add :: operations) tail
            let multiply = loop (res * x, Multiply :: operations) tail
            add @ multiply
        | _ -> [None]

    let res =
        equation.Numbers
        |> List.head

    let operations =
        equation.Numbers
        |> List.tail
        |> loop (res, [])
        |> List.choose id

    if operations = [] then None
    else Some (equation, operations)

let part1 filename =
    parseFile filename
    |> Seq.map solve
    |> Seq.choose id
    |> Seq.sumBy (fun (equation, _) -> equation.Result)