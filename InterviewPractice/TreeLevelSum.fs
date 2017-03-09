namespace CodeFightsProblem.InterviewPractice

  // This comes from a practice interview problem from codefights
  module internal TreeLevelSum =

    // A linear pass through the string is sufficient to
    //   sum all nodes at the kth level
    // This solution scored 300/300
    let treeLevelSum (tree:string) (k:int) =
      let mutable nums = [||] : string[]
      let mutable index = -1
      let mutable level = -1
      let mutable sum = 0
      let mutable num = ""
      (tree.ToCharArray())
      |> Array.iteri(fun i c ->
        if c = '(' then
          level <- level + 1
        elif c = ')' then
          level <- level - 1
        else
          if level = k then
            // if tree.[i-1] = '(' || tree.[i-1] = ')' then 
            //   sum <- sum + int(num)
            //   num <- ""
            // num <- num + (string c)
            if tree.[i-1] = '(' || tree.[i-1] = ')' then index <- index + 1
            if index >= (Array.length nums) then nums <- Array.append nums [| "" |]
            nums.[index] <- nums.[index] + (string c)
      )
      //sum
      nums |> Array.sumBy(int)

  module TreeLevelSumSimulator =
    let private inputs =
      [|
        "(0(5(6()())(14()(9()())))(7(1()())(23()())))", 2, 44;
        "(3(3()())(1()()))", 1, 4;
        "(0(5(6()())(4()(9()())))(7(1()())(3()())))", 2, 14;
        "(3()())", 3, 0;
        "(0(5()())())", 1, 5
      |]

    let simulate() =
      printfn "Running treeLevelSum simulator..."
      inputs
      |> Array.iter(fun (tree,k,expected) ->
         printfn "Inputs:\n\tTree: %s\n\tk: %d" tree k
         printfn "Expected: %d, Actual: %d" expected (TreeLevelSum.treeLevelSum tree k)
      )