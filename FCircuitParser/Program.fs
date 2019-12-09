namespace FCircuitParser

module BinaryPermutations =

    open System
    
    // Utilitiy functions for converting to and generating binary numbers.
    // I.e. n inputs have 2^n possible states. So binary numbers [0, 2^n) have to be generated for test cases.

    let binaryString x =
        let rec inner acc n =
            match n with
            | 0 -> acc
            | x -> inner ((x%2)::acc) (x/2)
        inner [] x

    let adjust length list =
        let dif = length - (list |> List.length)
        [1..dif] |> List.fold (fun s _ -> 0::s) list

    let binaryLength x =
        let rec inner acc n =
            match n with
            | 0 -> acc
            | x -> inner (acc+1) (x/2)
        inner 0 x

    /// Generate a list containing all of  the permutations of true/false lists with the specified length.
    let binaryList length =
        [for i in [0..((pown 2 length) - 1)] do
            yield binaryString i 
            |> adjust length 
            |> List.map (function | 1 -> true | _ -> false)]

    let limitedBinaryList length count =
        if length = 0 then [[]]
        else if length = 1 then [[true]; [false]] else
        let rng = new Random()
        let randTestNums = 
            Seq.initInfinite (fun _ -> rng.Next(0, (pown 2 length - 1)+1))
            |> Seq.distinct
            |> Seq.take count
        [for i in randTestNums do
            yield binaryString i 
            |> adjust length 
            |> List.map (function | 1 -> true | _ -> false)]

    let allInputStates n =
        if n = 0 then [[||]]
        else if n = 1 then [[|true|]; [|false|]] else
        binaryList n |> List.map List.toArray


module Parser =
    
    open FParsec
    open FParser
    open LogicGateDomain
    open System.IO
    open System.Collections.Generic
    open System
    open CircuitTypes

    // Checks if the parsed result returned two gates with the same name. This would lead to an incorrect pairing and a stackoverflow when trying to activate
        // the resulting loop.
    let areDuplicatedNames statements =
        statements
        |> Seq.tryFind(function | Connect (sName, tName, _) when sName = tName -> true | _ -> false)
        |> (function | Some (Connect(sName, tName, _)) -> failwithf "Gates %s and %s have conflicting names" sName tName | _ -> ())
            
    (*
    type Expr =
        | Value of bool
        | Variable of Identifier
        | PostFixOp of Expr * string
        | InfixOp of Expr * string * Expr
    *)

    (*
    Create big function to take a list of all the possible bool[]'s for the truth table and the valid inputs matched to each column.
    This will create Dictionary<bool[], bools>(BoolEqualityComparer)'s that map the input to each output function specified.
    Intermediate Maps from the input identifiers to each row for the inputs on the truth table.

    Failure modes:
        Undeclared output in the function √
        Undeclared input in the function √
        Wrong number of output functions for the number of declared outputs √

    Output lookup will be performed by doing outputF[InputStates]. Each input will be triggered and updated by the 

    
    Two inputs -> {false, false}, {false, true}, {true, false}, {true, true}
    A <- (In1.In2)';    -> Aoutput.Add({false, false}, true; {false, true}, true; {true, false}, true; {true, true}, false)
    B <- In1==In2;      -> Boutput.Add({false, false}, true; {false, true}, false; {true, false}, false; {true, true}, true)
    C <- In1 + In2';    -> Coutput.Add({false, false}, true; {false, true}, false; {true, false}, true; {true, true}, true)
    *)

    // outputFunctions is the [A <-..., B <-..., C <-...] above. (as "A", expr...; "B", expr...; "C", expr...;)
    // Map over these with the dictionary that represents the truth table for each function.
    let getTruthTable (inputNames: string list) (outputNames: string list) outputFunctions =
        let inputPerms = BinaryPermutations.allInputStates inputNames.Length

        //let inops = [".", 3; "+", 3; "=>", 2; "==", 2; "<>", 2] // And, Or, IfThen, XNor, XOr, Assign.
        
        let evalOutputFunctions expr (inputStates: bool[]) =
            let rec inner exp = 
                match exp with
                | Value b -> b
                | Variable identifier ->
                    match inputNames |> List.tryFindIndex ((=) identifier) with
                    | None -> failwithf "%s is not a declared input" identifier
                    | Some i -> inputStates.[i]
                | PostFixOp (e, str) -> 
                    match str with
                    | "'" -> not (inner e)
                    | _ -> failwithf "%s is not a falid postfix operator" str
                | InfixOp (e1, str, e2) ->
                    match str with
                    | "." -> (inner e1) && (inner e2)
                    | "+" -> (inner e1) || (inner e2)
                    | "=>" -> not (inner e1) || ((inner e1) && (inner e2))
                    | "==" -> (inner e1) = (inner e2)
                    | "<>" -> (inner e1) <> (inner e2)
                    | _ -> failwithf "%s is not a valid infix operator" str
            inner expr

        let functionMapper expr =
            let table = new Dictionary<bool[], bool>(new BoolArrayComparer())
            for bools in inputPerms do
                table.Add(bools, evalOutputFunctions expr bools)
            table
        // Pair output with its logic function as a truth table.
        let out = 
            outputFunctions |> List.map (fun (outputLabel, f) -> 
            match outputNames |> List.contains outputLabel with
            | false -> failwithf "%s is not a declared output" outputLabel
            | true -> outputLabel, (functionMapper f))
        // Ensure that the truth tables are arranged in the order in which the outputs are declared.
        outputNames 
        |> List.map 
            (fun name -> 
                match out |> List.tryFind (fun (n, f) -> n = name) with 
                | None -> failwith "Error matching function to proper output"
                | Some (_, func) -> name, func)
        |> List.toArray

    let getProgram (path:string) =
        use sr = new StreamReader(path)
        let text = sr.ReadToEnd() + "\n"

        match FParser.getProgram text with
        | Success (result,_,_) ->
            let codeStrList = new List<Statement>()
            for i in result do
                codeStrList.Add(i)
            codeStrList
        
        | Failure (msg,_,_) -> raise (new LogicGateDomain.ParserErrorException(msg))

    let getCircuit statements =
        let circuit = new List<LogicGate>()
        let mutable typeList = new ResizeArray<GateType>()

        let rec inner statements = 

            areDuplicatedNames statements // Check if there are any duplicates. If so, throw an exception.

            statements |> Seq.iter (printfn "%A"); printfn "";

            for cmd in statements do
                printfn "Checking: %A" cmd
                match cmd with
                | Comment -> ()
                | Load filename -> printfn "Loading %s" filename; getProgram filename |> inner
                | Define (NaryGate (typeName, name, inputCount, inputNames, outputNames)) -> 
                    let namesCount = inputNames.Length
                    if namesCount > inputCount && typeName <> Gate.Multiplexer then failwithf "Gate %s has too many named inputs" name else
                    let inputList = inputNames |> ResizeArray<string> // Might be [] or [n1, ...]
                    let outputList = outputNames |> ResizeArray<string> // Might be [] or [n1, ...]

                    match typeName with
                    | Gate.And -> circuit.Add(new AndGate(name, inputCount, inputList))
                    | Gate.Or -> circuit.Add(new OrGate(name, inputCount, inputList))
                    | Gate.Xor -> circuit.Add(new XorGate(name, inputCount, inputList))                            
                    | Gate.Xnor -> circuit.Add(new XnorGate(name, inputCount, inputList))
                    | Gate.Nand -> circuit.Add(new NandGate(name, inputCount, inputList))
                    | Gate.Nor -> circuit.Add(new NorGate(name, inputCount, inputList))
                    | Gate.Decoder -> circuit.Add(new NDecoder(name, inputCount, inputList, outputList))
                    | Gate.Multiplexer -> circuit.Add(new Multiplexer(name, inputCount, inputList))
                    | Gate.JKFlipFlop -> circuit.Add(new JKFlipFlop(name, 2, inputList, outputList))
                    | _ -> failwithf "%A is not an NaryGate" typeName

                | Define (UnaryGate (typeName, name, outputNames)) ->
                    let outputList = outputNames |> ResizeArray<string> // Might be [] or [n1, ...]

                    match typeName with
                    | Gate.Output -> circuit.Add(new OutputGate(name))
                    | Gate.Not -> circuit.Add(new NotGate(name))
                    | Gate.Identity -> circuit.Add(new IdentityGate(name))
                    | Gate.True -> circuit.Add(new TrueGate(name))
                    | Gate.False -> circuit.Add(new FalseGate(name))
                    | Gate.Input -> circuit.Add(new InputGate(name, false))
                    | Gate.DFlipFlop -> circuit.Add(new DFlipFlop(name, outputList))
                    | Gate.TFlipFlop -> circuit.Add(new TFlipFlop(name, outputList))
                    | _ -> failwithf "%A is not an UnaryGate" typeName

                | Instantiate (gateType, gateName) ->
                    try

                        let typeFinder = new Predicate<GateType>(function | Custom (typeName, _, _, _) -> true | _ -> false)

                        let t = typeList.Find typeFinder

                        match t with
                        | Custom (typeName, inputNames, outputNames, outputFunctions) ->

                            let inNamesLength = inputNames.Length
                            let outNamesLength = outputNames.Length
                            if inNamesLength = 0 || outputNames.Length = 0 then failwith "Custom gate %s must define input and output names" else
                            if outNamesLength <> outputFunctions.Length 
                                then failwithf "The number of output functions does not match the number of inputs for custom gate %s" typeName
                            else
                                let truthTable = getTruthTable inputNames outputNames outputFunctions
                                truthTable |> Array.iter (fun (name, dict) -> printfn "%s %A" name dict)

                                // Temporary for debugging purposes. Make the original function do this from the beginning...
                                let truthTable = truthTable |> Array.map (fun (_,dict) -> dict)

                                let inputs = inputNames |> ResizeArray<string>
                                let outputs = outputNames |> ResizeArray<string>

                                circuit.Add(new CustomGate(gateName, inNamesLength, outNamesLength, inputs, outputs, truthTable))
                        | _ -> failwithf "You cannot instantiate non-Custom gates"

                    with
                    | :? ArgumentNullException -> failwithf "A custom gate of type %s could not be found" gateType

                | Define (Custom (name, inputNames, outputNames, outputFunctions)) ->

                    typeList.Add(Custom (name, inputNames, outputNames, outputFunctions))

                
                (*
                MyDecoder.H :- SumOr, 4
                MyDecoder.H :- SumOr, 4; CarryOr, 4

                ConnectList of sourceName * source.Output * (targetName * target.Input) list

                Trying to allow a gate's output to be connected to the input on many other gates with one statement
                sourceGate[.outputName] :- targetGate, targetInput list
                *)
                | ConnectList (sourceName, sourceOutput, targets) ->
                    printfn "Connecting %A" (sourceName, sourceOutput, targets)
                    // Search the circuit to find gates that are already defined
                    try
                        let sourceGate = circuit.Find(fun x -> x.Name = sourceName)
                        if sourceGate = null then failwithf "Source gate '%s' is not defined" sourceName

                        targets |> List.iter (fun (targetName, targetNode) ->
                    
                            try
                                let targetGate = circuit.Find(fun x -> x.Name = targetName)
                                if targetGate = null then failwithf "Source gate '%s' is not defined" sourceName

                                let intNode = 
                                    match targetNode with
                                    | NodeNumber n -> n
                                    | NodeName stringNode -> 
                                        let x = targetGate.GetIndexForInputName(stringNode)
                                        if x = -1 then failwithf "Gate %s does not have an input with the name %s" sourceName stringNode else x

                                match sourceOutput with
                                | None -> // Single output gate. sourceGate should only be and, or, not, etc.
                                    match sourceGate with
                                        | :? SingleOutputGate as s -> s.AddConnection(new OutputConnection(targetGate, intNode))
                                        | :? MultipleOutputGate as m -> failwithf "%s must specify an output node choice" m.Name
                                        | _ -> printfn "Error in Connect, OneOut, NodeChoice for %s" sourceGate.Name

                                | Some (outputChoice) -> // Multiple output gate. sourceGate should only be decoder, custom, etc.
                                    match sourceGate with
                                    | :? SingleOutputGate as s -> failwithf "%s does not have multiple outputs" s.Name
                                    | :? MultipleOutputGate as m -> 
                                        match outputChoice with // Match on the output node of the source gate second.
                                        | NodeNumber outputNodeNumber ->
                                            m.AddConnectionTo(new OutputConnection(targetGate, intNode), outputNodeNumber)
                                        | NodeName outputNodeName ->
                                            match m.GetIndexForOutputName(outputNodeName) with
                                            | x when x = -1 -> failwithf "Gate %s does not have an output with the name %s" m.Name outputNodeName
                                            | outputNumber -> m.AddConnectionTo(new OutputConnection(targetGate, intNode), outputNumber-1)
                                    | _ -> printfn "Error in Connect, ManyOut, InputNode for %s" sourceGate.Name
                    
                            with
                            | :? ArgumentNullException ->
                                raise (GateNotDefinedException("Target gate " + targetName + " is not defined"))
                        )
                    with 
                    | :? ArgumentNullException ->
                        raise (GateNotDefinedException("Source gate " + sourceName + " is not defined"))

                | Connect (sourceName, targetName, outputType) ->
                    printfn "Connecting %A" (sourceName, targetName, outputType)
                    // Search the circuit to find gates that are already defined
                    try
                        let sourceGate = circuit.Find(fun x -> x.Name = sourceName)
                        if sourceGate = null then failwithf "Source gate '%s' is not defined" sourceName

                        try
                            let targetGate = circuit.Find(fun y -> y.Name = targetName)
                            if targetGate = null then failwithf "Target gate '%s' is not defined" targetName

                            match outputType with
                            | OneOut targetNode ->

                                let intNode = 
                                    match targetNode with
                                    | NodeNumber n -> n
                                    | NodeName stringNode -> 
                                        let x = targetGate.GetIndexForInputName(stringNode)
                                        if x = -1 then failwithf "Gate %s does not have an input with the name %s" sourceName stringNode else x
                            
                                match sourceGate with
                                    | :? SingleOutputGate as s -> s.AddConnection(new OutputConnection(targetGate, intNode))
                                    | :? MultipleOutputGate as m -> failwithf "%s must specify an output node choice" m.Name
                                    | _ -> printfn "Error in Connect, OneOut, NodeChoice for %s" sourceGate.Name
                            
                            | ManyOut (targetNode, outputNode) ->

                                let intInput = 
                                    match targetNode with // Match on the target node first using integer or name indexing.
                                    | NodeNumber intInput -> intInput 
                                    | NodeName stringInput -> 
                                        let x = targetGate.GetIndexForInputName(stringInput)
                                        if x = -1 then failwithf "Gate %s does not have an input with the name %s" sourceName stringInput else x

                                match sourceGate with // Then match on the source node using integer or name indexing
                                | :? SingleOutputGate as s -> failwithf "%s does not have multiple outputs" s.Name
                                | :? MultipleOutputGate as m -> 
                                    match outputNode with // Match on the output node of the source gate second.
                                    | NodeNumber outputNodeNumber ->
                                        m.AddConnectionTo(new OutputConnection(targetGate, intInput), outputNodeNumber)
                                    | NodeName outputNodeName ->
                                        match m.GetIndexForOutputName(outputNodeName) with
                                        | x when x = -1 -> failwithf "Gate %s does not have an output with the name %s" m.Name outputNodeName
                                        | outputNumber -> m.AddConnectionTo(new OutputConnection(targetGate, intInput), outputNumber-1)
                                | _ -> printfn "Error in Connect, ManyOut, InputNode for %s" sourceGate.Name

                                    
                                // Input node range checking must be done in the constructors...
                            
                        with
                        | :? ArgumentNullException ->
                            raise (GateNotDefinedException("Target gate " + targetName + " is not defined"))
                    with 
                    | :? ArgumentNullException ->
                        raise (GateNotDefinedException("Source gate " + sourceName + " is not defined"))
            printfn "\nEnd of Parsing Stage\n\n"
        inner statements
        circuit
                    

    [<EntryPoint>]
    let main argv = 
        
        0
