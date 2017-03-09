namespace CodeFightsProblem

open System

  module Main =

    let modules : Map<string,(unit->unit)> =
      [
        "InterviewPractice", InterviewPractice.SimulatorHelper.chooseSimulator
      ]
      |> Map.ofList

    let chooseModule() =
      let mutable validChoice = false
      while not validChoice do
        printfn "Please choose a module: "
        modules
        |> Map.iter(fun k _ ->
          printfn "%s" k
        )
        printf "|> "
        let choice = System.Console.ReadLine()
        let modOp = Map.tryFind choice modules
        if modOp.IsNone then
          printfn "Please be exact!"
        else
          validChoice <- true
          modOp.Value()

    [<EntryPoint>]
    let main argv = 
      chooseModule()
      0 // return an integer exit code
