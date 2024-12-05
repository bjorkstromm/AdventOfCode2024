let (|Integer|_|) (str: string) =
    match System.Int32.TryParse str with
    | (true, v) -> Some v
    | _ -> None

let (|ParseRegex|_|) regex str =
    let m = System.Text.RegularExpressions.Regex(regex).Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

let (|OrderingRule|_|) (str: string) =
    match str with
    | ParseRegex @"^([0-9]+)\|([0-9]+)$" [Integer left; Integer right] -> Some (left, right)
    | _ -> None

let (|Update|_|) (str: string) =
    let intArray (str : string) =
        str.Split(',')
        |> Array.map int32

    match str with
    | ParseRegex @"^([0-9,]+,[0-9]+)$" [a] -> Some (intArray a)
    | _ -> None

let parse lines =
    let folder (rules, updates) line =
        match line with
        | OrderingRule (left, right) -> (left, right) :: rules, updates
        | Update update -> rules, update :: updates
        | _ -> rules, updates

    let (rules, updates) =
        lines
        |> Seq.fold folder ([], [])

    (rules |> Set.ofList, updates |> List.rev)

let parseFile filename =
    System.IO.File.ReadAllLines filename
    |> parse


let isValid rules update =
    let pairs num nums =
        [| for i in 0 .. Array.length nums - 1 -> (nums.[i], num) |]

    update
    |> Array.mapi (fun i num -> pairs num update.[i+1..])
    |> Array.concat
    |> Array.forall (fun (a, b) -> not (Set.contains (a, b) rules))


let part1 filename =
    let (rules, updates) = parseFile filename

    updates
    |> List.filter (isValid rules)
    |> List.map (fun update -> update.[update.Length / 2])
    |> List.sum