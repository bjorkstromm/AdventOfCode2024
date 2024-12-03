type Instruction =
    | Multiply of int * int

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = System.Text.RegularExpressions.Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ], str.Substring(m.Index + m.Length))
   else None

let (|Mul|_|) (str: string) =
   match str with
   | ParseRegex @"^mul\((-?\d+),\s*(-?\d+)\)" ([Integer w; Integer h], rest) -> Some (Multiply (w, h), rest)
   | _ -> None

let rec parse input =
    match input with
    | "" -> []
    | Mul (instr, rest) -> instr :: parse rest
    | _ -> parse (input.Substring(1))


let part1 filename =
    filename
    |> System.IO.File.ReadAllText
    |> parse
    |> List.sumBy (fun instr ->
        match instr with
        | Multiply (x, y) -> x * y
    )