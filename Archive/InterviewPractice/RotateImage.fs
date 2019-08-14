namespace CodeFights.InterviewPractice

  module internal RotateImage =

    let rotateImage a =
      a |> List.transpose |> List.map List.rev

  module RotateImageSimulator =

    let inputs =
      [
        (
          [
            [1; 2; 3]
            [4; 5; 6]
            [7; 8; 9]
          ],
          [
            [7; 4; 1]
            [8; 5; 2]
            [9; 6; 3]
          ]
        )
        (
          [
            [1]
          ],
          [
            [1]
          ]
        )
        (
          [
            [10; 9; 6; 3; 7]
            [6; 10; 2; 9; 7]
            [7; 6; 3; 8; 2]
            [8; 9; 7; 9; 9]
            [6; 8; 6; 8; 2]
          ],
          [
            [6; 8; 7; 6; 10]
            [8; 9; 6; 10; 9]
            [6; 7; 3; 2; 6]
            [8; 9; 8; 9; 3]
            [2; 9; 2; 7; 7]
          ]
        )
      ]

    let simulator() =
      printfn "Running rotateImage simulator..."
      inputs
      |> List.iter(fun (input,expected) ->
        printfn "Input: %A" input
        printfn "Expected: %A, Actual: %A" expected <|
          RotateImage.rotateImage input
      )