namespace CodeFights.InterviewPractice

module internal DigitTreeSum =

  type Tree<'a> =
    {
      Value  : 'a
      Left   : Tree<'a> option
      Right  : Tree<'a> option
    }

  let digitTreeSum (t:Tree<int> option) =
    let rec traverseTree' (t:Tree<int>) (cont: int64 list list -> int64 list list) : int64 list list =
      match t.Left, t.Right with
      | None, None -> cont <| [[int64 t.Value]]
      | Some l, None -> traverseTree' l (fun ls -> cont <| ls |> List.map(fun x -> List.append x [int64 t.Value]))
      | None, Some r -> traverseTree' r (fun rs -> cont <| rs |> List.map(fun x -> List.append x [int64 t.Value]))
      | Some l, Some r ->
        traverseTree' l (fun ls ->
          traverseTree' r (fun rs ->
            let ls = ls |> List.map(fun x -> List.append x [int64 t.Value])
            let rs = rs |> List.map(fun x -> List.append x [int64 t.Value])
            cont <| ls@rs
          )
        )
    match t with
    | Some t ->
      traverseTree' t id
      |> List.sumBy(fun xs ->
        xs
        |> List.mapi(fun i x -> x * (pown 10L i))
        |> List.sum
      )
    | _      -> 0L

module DigitTreeSumSimulator =

  open DigitTreeSum

  let private tree v l r : Tree<int> = {Value = v; Left = l; Right = r}

  let private inputs =
    [
      (
        Some <| tree
          1
          (Some <| tree
            0
            (Some <| tree
              3
              None
              None)
            (Some <| tree
              1
              None
              None))
          (Some <| tree
            4
            None
            None),
        218L
      )
    ]

  let simulator() =
    printfn "Running digitTreeSum simulator..."
    inputs
    |> List.iter(fun (input,expected) ->
      printfn "Input: %A" input
      printfn "Expected: %d, Actual: %d" expected <|
        DigitTreeSum.digitTreeSum input
    )
