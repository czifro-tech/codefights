namespace CodeFights

open System

  module Main =

    let modules : (string * (unit->unit)) list =
      [
        "InterviewPractice", InterviewPractice.SimulatorHelper.chooseSimulator
        "Quit", id
      ]

    let rec chooseModule() =
      printfn "Please choose a module: "
      modules
      |> List.iteri(fun i s -> printfn "    %d. %s" (i+1) (fst s))
      printf "|> "
      let choice = Console.ReadLine()
      let result =
        match Int32.TryParse choice with
        | true, choice ->
          modules
          |> List.tryItem (choice-1)
          |> function
          | Some (_, mod')   -> mod'(); Some()
          | None            -> None
        | _, _ -> None
      result
      |> Option.orElseWith(fun _ ->
        match modules |> List.tryFind (fst >> (=) choice) with
        | Some (_, mod') -> mod'(); Some()
        | None          -> None
      )
      |> function
      | Some _ -> ()
      | _ ->
        printfn "Invalid option: %s" choice
        chooseModule();

    [<EntryPoint>]
    let main argv =
      chooseModule()
      0 // return an integer exit code
