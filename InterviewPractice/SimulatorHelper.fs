namespace CodeFightsProblem.InterviewPractice

  // This is not part of the problems
  // This is only to help run the simulators
  module SimulatorHelper =

    let private simulators : Map<string,(unit->unit)>=
      [
        "wordLadder", WordLadderSimulator.simulate;
        "tripletSum", TripletSumSimulator.simulate;
        "treeLevelSum", TreeLevelSumSimulator.simulate
      ]
      |> Map.ofList

    let chooseSimulator() =
      let mutable validChoice = false
      while not validChoice do
        printfn "Please choose a simulator: "
        simulators
        |> Map.iter(fun k _ ->
          printfn "%s" k
        )
        printf "|> "
        let choice = System.Console.ReadLine()
        let simOp = Map.tryFind choice simulators
        if simOp.IsNone then
          printfn "Please be exact!"
        else
          validChoice <- true
          simOp.Value()
