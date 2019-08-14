namespace CodeFights.InterviewPractice

  // Requirements: Space complexity: O(1), Time complexity: O(n)
  module internal FirstNotRepeatingCharacter =

    let firstNotRepeatingCharacter (s:string) =
      s.ToCharArray()
      |> Array.mapi(fun i c -> c,i)
      |> Array.fold(fun charSet (c, i) ->
        charSet
        |> Map.tryPick(fun (c', pos) i' -> if c' = c then Some (pos, i') else None)
        |> Option.map(fun (pos, i') ->
          charSet
          |> Map.remove (c, pos)
          |> Map.add (c, pos) (i'+1)
        )
        |> Option.defaultWith(fun _ ->
          charSet
          |> Map.add (c, i) 1
        )
      ) Map.empty<(char * int), int>
      |> Map.toSeq
      |> Seq.sortBy (fst >> snd)
      |> Seq.tryFind(fun (_, i) -> i = 1)
      |> Option.map (fst >> fst >> string)
      |> Option.defaultValue "_"

    // A naive solution to create an array to represent the alphabet where
    // each element is a counter for the respective character.
    // This approach gives alphabetical preference when finding the character
    // with a counter of 1 and multiple characters meet this condition
    let firstNotRepeatingCharacter_naive (s:string) =
      let chars = Array.replicate 26 0
      s.ToCharArray()
      |> Array.iter(fun c -> chars.[(int c)-97] <- chars.[(int c)-97] + 1)
      chars
      |> Array.tryFindIndex(fun i -> i = 1)
      |> Option.map(fun i -> char (i+97))
      |> Option.defaultValue '_'
      |> string

  module FirstNotRepeatingCharacterSimulator =

    let inputs =
      [
        "abacabad", "c"
        "abacabaabacaba", "_"
        "z", "z"
        "bcb", "c"
        "bcccccccb", "_"
        "abcdefghijklmnopqrstuvwxyziflskecznslkjfabe", "d"
        "zzz", "_"
        "bcccccccccccccyb", "y"
        "xdnxxlvupzuwgigeqjggosgljuhliybkjpibyatofcjbfxwtalc", "d"
        "ngrhhqbhnsipkcoqjyviikvxbxyphsnjpdxkhtadltsuxbfbrkof", "g"
      ]

    let simulator() =
      printfn "Running firstNotRepeatingCharacter simulator..."
      inputs
      |> List.iter(fun (input,expected) ->
        printfn "Input: %s" input
        printfn "Expected: %s, Actual: %s" expected <|
          FirstNotRepeatingCharacter.firstNotRepeatingCharacter input
      )