namespace CodeFightsProblem.InterviewPractice

  // A recursive method is used to avoid mutable vars
  // This method scored 300/300
  module internal StreamValidation =

    let private shiftLeft i =
      int <| (byte i) <<< 1

    let private isset i x =
      let x = x |> uint32 |> int
      (x &&& i) <> 0

    let private mostSig = 0b10000000

    // Since UTF-8 uses the `n` most significant bits
    //  to represent the number of bytes a char is,
    //  checking and left shifting until a 0 is found
    //  suffices
    let private getByteCount x =
      let rec count x' b =
        if not b then 0
        else
          let nx = shiftLeft x'
          let nb = isset mostSig nx
          1 + count nx nb
      count x (isset mostSig x)

    // This is reursive to avoid mutable vars
    let rec private processStream stream =
      let n = Array.length stream
      if n = 0 then true
      else
        let byteCount = max (getByteCount stream.[0]) 1
        if byteCount > n then false
        elif byteCount = 1 && isset mostSig stream.[0] then false
        elif byteCount = 1 && not <| isset mostSig stream.[0] then
          processStream stream.[byteCount..]
        else
          let stream' = Array.take byteCount stream
          printfn "%d" byteCount
          printfn "%A" stream'
          (stream'.[1..]
          |> Array.mapi(fun i s ->
            isset mostSig s && not <| isset mostSig (shiftLeft s)
          )
          |> Array.reduce (&&)) && processStream stream.[byteCount..]

    let streamValidation (stream:int[]) =
      processStream stream

  module StreamValidationSimulator =

    let private inputs =
      [
        [|10|],true;
        [|145|],false;
        [|240;162;138;147;145|],false;
        [|115;100;102;231;154;132;13;10|],true;
        [|39;89;227;83;132;95;10;0|],false
      ]

    let simulate() =
      printfn "Running streamValidation simulator..."
      inputs
      |> List.iter(fun (stream,expected) ->
        printfn "Input: %A" stream
        printfn "Expected: %b, Actual: %b" expected (StreamValidation.streamValidation stream)
      )