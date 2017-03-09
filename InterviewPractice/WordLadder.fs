namespace CodeFightsProblem.InterviewPractice

  module internal WordLadder =

    // We can only transform a word by 1 character
    // So we need to know the distance between two words
    let getLevenshteinDistance (w1:string) (w2:string) =
      if w1 = "" || w2 = "" then
        System.Int32.MaxValue
      else
        w1.ToCharArray()
        |> Array.map2(fun c1 c2 -> if c1 = c2 then 0 else 1) (w2.ToCharArray())
        |> Array.sum

    (*
      b: hit, e: cog, l: hot, dot, dog, lot, log, cog
      Graph as map:
        [
          hit, [hot]
          hot, [hit,dot,lot]
          dot, [hot,lot,dog]
          dog, [dot,log,cog]
          lot, [hot,dot,log]
          log, [lot,dog,cog]
          cog, [dog,log]
        ]
    *)
    module Solution =
      // We are converting wordList to match
      //  above schema
      let convertToGraph wordList =
        let nodes =
          wordList
          |> Array.toList
        nodes
        |> List.map(fun w ->
          let children =
            nodes
            |> List.filter(fun w' -> (getLevenshteinDistance w' w) = 1 )
          w,(false,children)
        )
        |> Map.ofList
      // A simple breadth first search is the best answer.
      // Keeping track of the minDepth every time we land on
      //  endWord will ensure we return the shortest path length
      // This scored 300/300
      let bfs b e (g:Map<string, bool*(string list)>) =
        let mutable g = g
        let mutable q : (string*int) list = []
        let nq = q
        let loc = b,0
        q <- loc::q
        let visit k =
          let _,c = g.[k]
          g <- Map.remove k g
          g <- Map.add k (true,c) g
        let mutable minDepth = 0
        while not (q |> List.isEmpty) do
          let h = List.head q
          let k,(d:int) = h
          if k = e then
            if minDepth > 0 then minDepth <- (if d < minDepth then d else minDepth)
            else minDepth <- d
          q <- List.tail q
          let _,c = g.[k]
          visit k
          let c = 
            c
            |> List.filter(fun w ->
              let v,_ = g.[w]
              not v
            )
            |> List.map(fun w -> w,d+1)
          q <- q @ c
        minDepth

    // Note: these solutions do not pass all tests
    //  due to time constraint
    module AlternativeSolutions =
      (*
        b: hit, e: cog, l: hot, dot, dog, lot, log, cog
        Graph as map:
          [
            hit, [(hot, 1)]
            hot, [(hit, 1), (dot, 1), (lot, 1)]
            dot, [(hot, 1), (lot, 1), (dog, 1)]
            dog, [(dot, 1), (log, 1), (cog, 1)]
            lot, [(hot, 1), (dot, 1), (log, 1)]
            log, [(lot, 1), (dog, 1), (cog, 1)]
            cog, [(dog, 1), (log, 1)]
          ]
      *)
      // This is different graph
      //  keeps distance between node and children
      let convertToGraph wordList =
        let nodes =
          wordList
          |> Array.toList
          |> List.map(fun w -> w,1)
        nodes
        |> List.map(fun (w,_) ->
          let children =
            nodes
            |> List.filter(fun (w',_) -> (getLevenshteinDistance w' w) = 1)
            |> Map.ofList
          w,children
        )
        |> Map.ofList

      // Compute all shortest paths from b
      // Lookup shortest path to e
      // This scored 214/300, too slow
      let shortestPathBetween (g:Map<string, Map<string,int> >) b e =
        let rec searchForShortestPath curr dist (visited:string list) accMap =
          let visit m =
            (m, g.[curr])
            ||> Map.fold(fun acc w d -> searchForShortestPath w (d + dist) (visited@ [w] ) acc)
          match Map.tryFind curr accMap with
          | None -> accMap |> Map.add curr (dist, visited) |> visit
          | Some x ->
            let shortestKnownPath,_ = x
            if dist < shortestKnownPath then
              accMap |> Map.add curr (dist, visited) |> visit
            else accMap
        let shortestPaths = searchForShortestPath b 0 [] Map.empty
        let path = shortestPaths |> Map.tryFind e
        if path.IsNone then (0,[]) else path.Value

      // Does not use a graph, rather recursively does DFS
      // This scored 120/300, too slow
      let rec calculateShortestPath wordList parentWord keyWord =
        if Array.isEmpty wordList then
          if parentWord = keyWord then 1 else 0
        elif parentWord = keyWord || (getLevenshteinDistance keyWord parentWord) = 1 then 1
        else
          let pathDistances =
            wordList
            |> Array.map(fun w ->
              if (getLevenshteinDistance w parentWord) <> 1 then 0
              else
                let subList = wordList |> Array.filter(fun w' -> w' <> w)
                let distance = calculateShortestPath subList w keyWord
                distance
            )
            |> Array.filter(fun d -> d <> 0)
          if pathDistances |> Array.isEmpty then 0 else (Array.min pathDistances) + 1

  module WordLadderSimulator =

    let private inputs =
      [|
        "hit", "cog", [| "hot"; "dot"; "dog"; "lot"; "log"; "cog" |], 5;
        "hit", "cog", [| "hot"; "dot"; "dog"; "lot"; "log" |], 0;
        "a", "c", [| "a"; "b"; "c"; |], 2;
        "hot", "dog", [| "hot"; "dog" |], 0;
        "hot", "dog", [| "hot"; "cog"; "dog"; "tot"; "hog"; "hop"; "pot"; "dot" |], 3;
        "hot", "dog", [| "hot"; "dog"; "cog"; "pot"; "dot" |], 3;
        "hit", "cog", [| "hot"; "dot"; "dog"; "hit"; "lot"; "log"; "cog" |], 5;
        "lost", "cost", [| "most"; "fish"; "lost"; "cost"; "fish" |], 2;
        "talk", "tail", [| "talk"; "tons"; "fall"; "tail"; "gale"; "hall"; "negs" |], 0;
        "kiss", "tusk", [| "miss"; "dusk"; "kiss"; "musk"; "tusk"; "diss"; "disk"; "sang"; "ties"; "muss" |], 5;
        "teach", "place", [| "peale"; "wilts"; "place"; "fetch"; "purer"; "pooch"; "peace"; "poach"; "berra"; "teach"; "rheum"; "peach" |], 4
      |]

    let private solutions : Map<string,(string->string->string[]->int)> =
      [
        "bfs", (fun beginWord endWord wordList ->
                 (WordLadder.Solution.convertToGraph wordList) |> WordLadder.Solution.bfs beginWord endWord
                 )
        "shortestPathBetween", (fun beginWord endWord wordList -> 
                                  let d,_ = WordLadder.AlternativeSolutions.shortestPathBetween (WordLadder.AlternativeSolutions.convertToGraph wordList) beginWord endWord
                                  d
                                )
        "calculateShortestPath", (fun beginWord endWord wordList -> WordLadder.AlternativeSolutions.calculateShortestPath wordList beginWord endWord)
      ]
      |> Map.ofList

    let simulate() =
      let mutable validChoice = false
      while not validChoice do
        printfn "Please choose a solution: "
        solutions
        |> Map.iter(fun k _ ->
          printfn "%s" k
        )
        printf "|> "
        let choice = System.Console.ReadLine()
        let solOp = Map.tryFind choice solutions
        if solOp.IsNone then
          printfn "Please be exact!"
        else
          validChoice <- true
          let solution = solOp.Value
          inputs
          |> Array.iter(fun (b,e,l,expected) ->
            let actual =
              if not (Array.exists(fun w -> w = e) l) then 0
              else
                let l = Array.append [| b |] l
                let res = solution b e l
                if res = 0 then 0 else res+1 // need to add 1 to include beginWord
            printfn "Inputs:\n\tbeginWord: %s\n\tendWord: %s\n\twordList: %A" b e l
            printfn "Expected: %d, Actual: %d" expected actual
          )