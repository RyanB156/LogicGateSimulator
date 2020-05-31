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

    // | ConnectList of Identifier * NodeChoice option * (Identifier * NodeChoice) list

    let areDuplicatedNames statements =
        statements
        |> Seq.iter (fun s -> 
            match s with
            | ConnectList (sName, _, list) ->  
                if list |> List.map fst |> List.contains sName
                then failwithf "Gates %s and %s have conflicting names" sName sName
            | _ -> ())
        

    
    // Creates an array of string * Dictionary<int, bool> pairs that represent the truth table for each of the functions that have a matching
    //  name in "outputNames"
    // The remaining functions are taken as user defined variables to simplify large expression and reduce code repetition.
    // First, a list of all possible input states is generated based on the length of "inputNames"
    // Second, the list of all input states is iterated through and evaluated against each function. If the function is for an output then its value
    //  is put in "outputs" otherwise its value is put in "variables". 
    //  These are inserted as dict[var|out name][inputState] = (inputState, f(inputState))
    // Input states are stored in the dictionaries as an integer to make lookups easier
    let getTruthTable (inputNames: string list) (outputNames: string list) functions =

        let mintermDigits bs = bs |> Array.fold (fun s t -> s * 2 + if t then 1 else 0) 0

        let inputPerms = BinaryPermutations.allInputStates inputNames.Length

        let variables = new Dictionary<string, Dictionary<int, bool>>() // Mapping of variable name to its truth table.
        let outputs = new Dictionary<string, Dictionary<int, bool>>()
        

        let evalOutputFunctions expr (inputStates: bool[]) =
            let rec inner exp = 
                match exp with
                | Value b -> b
                | Variable identifier ->
                    match inputNames |> List.tryFindIndex ((=) identifier) with
                    | None -> 
                        match variables.ContainsKey identifier with
                        | false -> failwithf "%s is not a declared input or variable in %A" identifier expr
                        | true -> 
                            printfn "Looking up value for %s as variable" identifier
                            try variables.[identifier].[mintermDigits inputStates]
                            with :? KeyNotFoundException as e -> failwith e.Message
                        // ^ *** Change this to pull value for a variable.
                    | Some i -> inputStates.[i]
                | PostFixOp (e, str) -> 
                    match str with
                    | "'" -> not (inner e)
                    | _ -> failwithf "%s is not a valid postfix operator in %A" str expr
                | InfixOp (e1, str, e2) ->
                    match str with
                    | "." -> (inner e1) && (inner e2)
                    | "+" -> (inner e1) || (inner e2)
                    | "=>" -> not (inner e1) || ((inner e1) && (inner e2))
                    | "==" -> (inner e1) = (inner e2)
                    | "<>" -> (inner e1) <> (inner e2)
                    | _ -> failwithf "%s is not a valid infix operator in %A" str expr
            inner expr

        (* 
        
            Issues:
                Differentiating between inputs and variables
                    
        *)

        let newWay = 
            outputNames |> List.iter (fun str -> outputs.Add(str, new Dictionary<int, bool>()))
            inputPerms |> List.iter (fun inputs ->
                variables.Clear() // Remove stored values for variables
                for (n, f) in functions do
                    let b = evalOutputFunctions f inputs
                    if outputs.ContainsKey n then 
                        outputs.[n].Add(mintermDigits inputs, b)
                    else 
                        if variables.ContainsKey n |> not then variables.Add(n, new Dictionary<int, bool>()) else ()
                        variables.[n].Add(mintermDigits inputs, b)
                    |> ignore
            )
            [| for key in outputs.Keys do yield (key, outputs.[key]) |]

        newWay

    let getProgram (path:string) =
        use sr = new StreamReader(path)
        let text = sr.ReadToEnd() + "\n"

        match FParser.parseProgram text with
        | Success (result,_,_) ->
            let codeStrList = new List<Statement>()
            for i in result do
                codeStrList.Add(i)
            codeStrList
        
        | Failure (msg,_,_) -> raise (new LogicGateDomain.ParserErrorException(msg))

    let getProgramFromString (program:string) =
        let program = program + "\n"
        match FParser.parseProgram program with
        | Success (result,_,_) ->
            let codeStrList = new List<Statement>()
            for i in result do
                codeStrList.Add(i)
            codeStrList
        
        | Failure (msg,_,_) -> raise (new LogicGateDomain.ParserErrorException(msg))

    let getCircuit statements =
        let circuit = new List<LogicGate>()
        let typeList = new ResizeArray<GateType>()

        let rec inner statements = 

            areDuplicatedNames statements // Check if there are any duplicates. If so, throw an exception.

            statements |> Seq.iter (printfn "%A"); printfn "";

            for cmd in statements do
                printfn "Checking: %A" cmd
                match cmd with
                | Comment -> ()
                | Load filePath -> printfn "Loading %s" filePath; getProgram filePath |> inner
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
                        let typeFinder = new Predicate<GateType>(function | Custom (_, _, _, _) -> true | _ -> false)
                        let t = typeList.Find typeFinder

                        match t with
                        | Custom (_, inputNames, outputNames, outputFunctions) ->

                            let inNamesLength = inputNames.Length
                            let outNamesLength = outputNames.Length
                            if inNamesLength = 0 || outputNames.Length = 0 then failwith "Custom gate %s must define input and output names" else
                            
                            // Ensure that there are no name conflicts between inputs and user-defined variables.
                            let outputsAssigned = outputFunctions |> List.map fst
                            for input in inputNames do
                                if outputsAssigned |> List.contains input then failwithf "%s cannot be defined as a variable. %s already exists as an input" input input
                            // Ensure that there are no name conflicts between outputs and user-defined variables.
                            outputsAssigned
                            |> List.sort
                            |> List.fold (fun s t -> if s = t then failwithf "%s cannot be assigned to more than once" s else t) ""
                            |> ignore

                            let truthTable = getTruthTable inputNames outputNames outputFunctions
                            truthTable |> Array.iter (fun (name, dict) -> printfn "%s %A" name dict)

                            let truthTable = truthTable |> Array.map (fun (_,dict) -> dict)

                            // Convert F# list to C# generic list.
                            let inputs = inputNames |> ResizeArray<string>
                            let outputs = outputNames |> ResizeArray<string>

                            circuit.Add(new CustomGate(gateName, inNamesLength, outNamesLength, inputs, outputs, truthTable))
                        | _ -> failwithf "You cannot instantiate non-Custom gates"

                    with
                    | :? ArgumentNullException -> failwithf "A custom gate of type %s could not be found" gateType

                | Define (Custom (name, inputNames, outputNames, outputFunctions)) ->
                    typeList.Add(Custom (name, inputNames, outputNames, outputFunctions))

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
                            
            printfn "\nEnd of Parsing Stage\n\n"
        inner statements
        circuit
                    

    [<EntryPoint>]
    let main argv = 
        
        0
