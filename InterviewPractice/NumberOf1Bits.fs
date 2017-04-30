namespace CodeFightsProblem.InterviewPractice

  // Requirement: Time Complexity of O(k) where k is number of 1 bits
  //
  module internal NumberOf1Bits =

    let rec numberOf1Bits (n:int) =
      if n = 0 then 0
      else
        1 + (numberOf1Bits (n &&& ~~~(-n)))

  module NumberOf1BitsSimulator =

    let inputs =
      [
        13,3;
        0,0;
        2,1;
        648,3;
        2989,8;
        84618,7;
        939615,13;
        2100224,3;
        28732358,15;
        865600010,10
      ]

    let simulator() =
      printfn "Running numberOf1Bits simulator..."
      inputs
      |> List.iter(fun (input,expected) ->
        printfn "Input: %d" input
        printfn "Expected: %d, Actual: %d" expected (NumberOf1Bits.numberOf1Bits input)
      )