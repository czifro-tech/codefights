namespace CodeFights.InterviewPractice

module FirstDuplicate =

  let firstDuplicate a =
    a
    |> Seq.mapi(fun i x -> (x,i))
    |> Seq.groupBy fst
    |> Seq.filter(fun (_, g) -> g |> Seq.length > 1)
    |> Seq.map(fun (_, g) -> g |> Seq.item 1)
    |> fun seq ->
      match seq |> Seq.isEmpty with
      | true -> -1
      | _ ->
        seq |> Seq.minBy(fun (_, i) -> i) |> fun (n, _) -> n

module FirstDuplicateSimulator =

  let inputs =
    [
      (
        [2; 1; 3; 5; 3; 2],
        3
      )
      (
        [2; 2],
        2
      )
      (
        [2; 4; 3; 5; 1],
        -1
      )
    ]

  let simulator() =
    printfn "Running firstDuplicate simulator..."
    inputs
    |> List.iter(fun (input,expected) ->
      printfn "Input: %A" input
      printfn "Expected: %d, Actual: %d" expected <|
        FirstDuplicate.firstDuplicate input
    )