namespace CodeFights.InterviewPractice

module internal TraverseTree =

  type Tree<'a> =
    {
      Value  : 'a
      Left   : Tree<'a> option
      Right  : Tree<'a> option
    }

  let traverseTree (t:Tree<int> option) =
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
    | Some t -> traverseTree' t id |> List.collect id
    | _      -> List.empty

module TraverseTreeSimulator =

  open TraverseTree

  let private tree v l r : Tree<int> = {Value = v; Left = l; Right = r}

  let private inputs =
    [
      (
        Some <| tree
          1
          (Some <| tree
            2
            None
            (Some <| tree
              3
              None
              None))
          (Some <| tree
            4
            (Some <| tree
              5
              None
              None)
            None),
        [1; 2; 4; 3; 5]
      )
    ]

  let simulator() =
    printfn "Running traverseTree simulator..."
    inputs
    |> List.iter(fun (input,expected) ->
      printfn "Input: %A" input
      printfn "Expected: %A, Actual: %A" expected <|
        TraverseTree.traverseTree input
    )
