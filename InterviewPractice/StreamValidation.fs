namespace CodeFights.InterviewPractice

  // A recursive method is used to avoid mutable vars
  // This method scored 300/300
  module internal StreamValidation =

    let private shiftLeft i = int <| (byte i) <<< 1

    let private isset x = (x &&& 0b10000000) <> 0

    let private validByte s = isset s && not <| isset (shiftLeft s)

    let private validSeq = (Array.map validByte) >> (Array.reduce (&&))

    // Since UTF-8 uses the `n` most significant bits
    //  to represent the number of bytes a char is,
    //  checking and left shifting until a 0 is found
    //  suffices
    let rec private getByteCount x =
      if not <| isset x then 0 else 1 + getByteCount (shiftLeft x)

    // This is recursive to avoid mutable vars
    let rec streamValidation stream =
      if Array.isEmpty stream then true
      else
        let bc = max (getByteCount stream.[0]) 1
        if bc > Array.length stream then false
        elif bc = 1 then (not <| isset stream.[0]) && streamValidation stream.[1..]
        else (validSeq stream.[1..bc-1]) && streamValidation stream.[bc..]


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