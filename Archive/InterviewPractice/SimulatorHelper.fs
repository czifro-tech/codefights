namespace CodeFights.InterviewPractice

  // This is not part of the problems
  // This is only to help run the simulators
  module SimulatorHelper =

    let private simulators : (string * (unit->unit)) list =
      [
        "wordLadder", WordLadderSimulator.simulate;
        "tripletSum", TripletSumSimulator.simulate;
        "treeLevelSum", TreeLevelSumSimulator.simulate;
        "streamValidation", StreamValidationSimulator.simulate;
        "numberOf1Bits", NumberOf1BitsSimulator.simulator;
        "firstNotRepeatingCharacter", FirstNotRepeatingCharacterSimulator.simulator;
        "rotateImage", RotateImageSimulator.simulator;
        "firstDuplicate", FirstDuplicateSimulator.simulator;
        "traverseTree", TraverseTreeSimulator.simulator;
        "largestValuesInTreeRows", LargestValuesInTreeRowsSimulator.simulator;
        "digitTreeSum", DigitTreeSumSimulator.simulator;
        "quit", id;
      ]

    let rec chooseSimulator() =
      printfn "Please choose a simulator: "
      simulators
      |> List.iteri(fun i s -> printfn "    %d. %s" (i+1) (fst s))
      printf "|> "
      let choice = System.Console.ReadLine()
      let result =
        match System.Int32.TryParse choice with
        | true, choice ->
          simulators
          |> List.tryItem (choice-1)
          |> function
          | Some (_, sim)   -> sim(); Some()
          | None            -> None
        | _, _ -> None
      result
      |> Option.orElseWith(fun _ ->
        match simulators |> List.tryFind (fst >> (=) choice) with
        | Some (_, sim) -> sim(); Some()
        | None          -> None
      )
      |> function
      | Some _ -> ()
      | _ ->
        printfn "Invalid option: %s" choice
        chooseSimulator();
