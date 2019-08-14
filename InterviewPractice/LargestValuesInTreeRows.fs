namespace CodeFights.InterviewPractice

module internal LargestValuesInTreeRows =

  type Tree<'a> =
    {
      Value  : 'a
      Left   : Tree<'a> option
      Right  : Tree<'a> option
    }

  let largestValuesInTreeRows (t:Tree<int> option) =
    let rec traverseTree' (t:Tree<int>) (cont: int list list -> int list list) : int list list =
      match t.Left, t.Right with
      | None, None -> cont <| [[t.Value]]
      | Some l, None -> traverseTree' l (fun ls -> cont <| ls |> List.map(fun x -> List.append x [t.Value]))
      | None, Some r -> traverseTree' r (fun rs -> cont <| rs |> List.map(fun x -> List.append x [t.Value]))
      | Some l, Some r ->
        traverseTree' l (fun ls ->
          traverseTree' r (fun rs ->
            let ls = ls |> List.map(fun x -> List.append x [t.Value])
            let rs = rs |> List.map(fun x -> List.append x [t.Value])
            cont <| ls@rs
          )
        )
    match t with
    | Some t -> traverseTree' t id |> List.map List.max
    | _      -> List.empty

module LargestValuesInTreeRowsSimulator =

  open LargestValuesInTreeRows

  let private tree v l r : Tree<int> = {Value = v; Left = l; Right = r}

  let private inputs =
    [
      (
        Some <| tree
          -1
          (Some <| tree
            5
            None
            None)
          (Some <| tree
            7
            None
            (Some <| tree
              1
              None
              None)),
        [-1; 7; 1]
      )
    ]

  let simulator() =
    printfn "Running largestValuesInTreeRows simulator..."
    inputs
    |> List.iter(fun (input,expected) ->
      printfn "Input: %A" input
      printfn "Expected: %A, Actual: %A" expected <|
        LargestValuesInTreeRows.largestValuesInTreeRows input
    )
