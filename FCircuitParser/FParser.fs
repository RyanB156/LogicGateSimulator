namespace FCircuitParser

module FParser =

(*
    <GateType>[input count][(In1Name, In2Name, ..., InNName [: Out1Name, ..., OutNName])]
    <source gate name>[.<output number> (0 indexed)] :- <target gate name>[, <target gate input choice> (1 indexed)]
    create <gateName>(In1Name, ..., InNName : Out1Name, ..., OutNName)
    {
        Out1Name <- (Boolean expression using [', ., +, =>, ==, <>]) ; [// <comment>]
        ..., OutNName <- ... ;
    }
    [// <comment>]
    [/* <block comment> */]

    Multiplexer: Inputs 1, 2, 3, 4, 5, 6 (1, 2 controls) (3-6 inputs)
*)

    open FParsec

    module CircuitTypes =

        type Gate = 
            | And
            | Or
            | Xor
            | Not
            | Input
            | Output
            | Xnor
            | Nand
            | Nor
            | Identity
            | True
            | False
            | Decoder
            | Multiplexer
            | DFlipFlop
            | TFlipFlop
            | JKFlipFlop


        // Define valid gate types for the parser.
        let ioGates = [ "Input", Gate.Input; "Output", Gate.Output ]
        let unaryGates = [ "Identity", Gate.Identity; "True", Gate.True; "False", Gate.False; "Not", Gate.Not; "DFlipFlop", DFlipFlop; "TFlipFlop", TFlipFlop]
        let nAryGates = [ "And", Gate.And; "Or", Gate.Or; "Xor", Gate.Xor; "Xnor", Gate.Xnor; "Nand", Gate.Nand; "Nor", 
                            Gate.Nor; "Decoder", Gate.Decoder; "Multiplexer", Gate.Multiplexer; "JKFlipFlop", Gate.JKFlipFlop]  

        let gateTypePairs = ioGates @ unaryGates @ nAryGates
    
        let unaryGatePairs = ioGates @ unaryGates
        let nAryGatePairs = nAryGates

        let allGateNames = ioGates @ unaryGates @ nAryGates |> List.map fst |> ResizeArray<string>
        //

        type Identifier = string
        type InputNode = int
        type OutputNode = int
        type InputCount = int

        type NodeChoice = | NodeNumber of int | NodeName of string

        type Inputs = Identifier list
        type Outputs = Identifier list

        type Expr =
            | Value of bool
            | Variable of Identifier
            | PostFixOp of Expr * string
            | InfixOp of Expr * string * Expr

        type OutputFunction = Identifier * Expr

        type GateType =
            | UnaryGate of Gate * Identifier * Outputs
            | NaryGate of Gate * Identifier * InputCount * Inputs * Outputs
            | Custom of Identifier * Inputs * Outputs * OutputFunction list

        type OutputType =
            | OneOut of NodeChoice
            | ManyOut of NodeChoice * NodeChoice // Connect to Input node from Output node.

        type Statement = 
            | Comment
            | Define of GateType
            | Connect of Identifier * Identifier * OutputType

    module IndentationParserWithoutBacktracking =

        let tabStopDistance = 4

        type LastParsedIndentation() =
            [<DefaultValue>]
            val mutable Value: int32
            [<DefaultValue>]
            val mutable EndIndex: int64

        type UserState = 
            {Indentation: int;
                LastParsedIndentation: LastParsedIndentation}
            with
                static member Create() = {Indentation = -1;
                                            LastParsedIndentation = LastParsedIndentation(EndIndex = -1L)}

        type CharStream = CharStream<UserState>
        type Parser<'t> = Parser<'t, UserState>

        // If this function is called at the same index in the stream
        // where the function previously stopped, then the previously
        // returned indentation will be returned again. 
        // This way we can avoid backtracking at the end of indented blocks.
        let skipIndentation (stream: CharStream) =    
            let lastParsedIndentation = stream.UserState.LastParsedIndentation
            if lastParsedIndentation.EndIndex = stream.Index then
                lastParsedIndentation.Value
            else
                let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
                while stream.Peek() = '#' do
                    stream.SkipRestOfLine(false) // skip comment
                    indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)
                lastParsedIndentation.EndIndex <- stream.Index
                lastParsedIndentation.Value <- indentation
                indentation

        let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
            fun stream ->
                let oldIndentation = stream.UserState.Indentation
                let indentation = skipIndentation stream
                if indentation <= oldIndentation then 
                    Reply(Error, expected (if indentation < 0 then "newline" else "indented " + label))
                else
                    stream.UserState <- {stream.UserState with Indentation = indentation}            
                    let results = ResizeArray()
                    let mutable stateTag = stream.StateTag
                    let mutable reply = p stream // parse the first element
                    let mutable newIndentation = 0
                    while reply.Status = Ok 
                            && (results.Add(reply.Result)
                                newIndentation <- skipIndentation stream
                                newIndentation = indentation)
                        do
                            stateTag <- stream.StateTag
                            reply <- p stream
                    if reply.Status = Ok 
                        || (stream.IsEndOfStream && results.Count > 0 && stream.StateTag = stateTag) 
                    then
                        if newIndentation < indentation || stream.IsEndOfStream then
                            stream.UserState <- {stream.UserState with Indentation = oldIndentation}
                            Reply(List.ofSeq results)
                        else
                            Reply(Error, messageError "wrong indentation")
                    else // p failed
                        Reply(reply.Status, reply.Error) 

        let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
            fun stream ->
                printfn "%A: Entering %s" stream.Position label
                let reply = p stream
                printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
                reply

        let isBlank = fun c -> c = ' ' || c = '\t'
        let ws1 = skipMany1SatisfyL isBlank "whitespace"
        let pcomment = pstring "//" >>. many1Satisfy ((<>) '\n')
        let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces) |>> fun _ -> ()
        let pmlcomment = pstring "/*" >>. many1CharsTill anyChar (pstring "*/")

        let str_ws s = pstring s .>> spaces
        let str_wsopt s = pstring s .>> opt spaces
        let str_ws1 s = pstring s .>> spaces1

        let ws = pspaces >>. many (pspaces >>. pmlcomment >>. pspaces) |>> fun _ -> ()
        let wsopt = optional ws |>> fun _ -> ()
        let blockcomment = between (pstring "/*") (pstring "*/") (charsTillString "*/" false System.Int32.MaxValue) |>> (fun _ -> ())
        let comment = (pstring "//" >>. skipRestOfLine false <|> blockcomment)
        let wsBeforeEOL = skipManySatisfy isBlank >>. optional comment .>> spaces
        let wsBeforeEOLBlank = wsBeforeEOL .>> (many1 newline)
        //let identifier = manySatisfy (function | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') -> true | _ -> false)

        let identifier =
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
            //<!> "Identifier"


        let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str        

        

    open IndentationParserWithoutBacktracking
    open CircuitTypes

    let indentedStatements, indentedStatementsRef = createParserForwardedToRef()

    // pexpr
    let (pexpr : Parser<Expr,UserState>), pexprimpl = createParserForwardedToRef ()

    type Assoc = Associativity

    let opp = OperatorPrecedenceParser<Expr,unit,UserState>()

    let pliteral = (pstring "1" |>> fun _ -> (Value true)) <|> (pstring "0" |>> fun _ -> (Value false))
    let pvalue = pliteral <|> (identifier .>> wsopt |>> fun str -> Variable str)

    let term = between (str_wsopt "(") (str_wsopt ")") pexpr <|> pvalue
    opp.TermParser <- term

    let inops = [".", 4; "+", 3; "=>", 2; "==", 2; "<>", 2] // And, Or, IfThen, XNor, XOr, Assign.
    for (op, d) in inops do
        opp.AddOperator(InfixOperator(op, wsopt, d, Assoc.Left, fun x y -> InfixOp(x, op, y)))
    let postops = ["'"] // Negation.
    for op in postops do
        opp.AddOperator(PostfixOperator(op, wsopt, 4, true, fun x -> PostFixOp(x, op)))

    pexprimpl := opp.ExpressionParser
    // end pexpr

    let gateMatch : Parser<Gate,UserState> = gateTypePairs |> List.map (fun (s,g) -> stringReturn s g) |> List.reduce (<|>)
    let unaryGateMatch : Parser<Gate,UserState> = unaryGatePairs |> List.map (fun (s,g) -> stringReturn s g) |> List.reduce (<|>)
    let nAryGateMatch : Parser<Gate,UserState> = nAryGatePairs |> List.map (fun (s,g) -> stringReturn s g) |> List.reduce (<|>)

    // Optional names
    let pInputNames = (sepBy (str_wsopt " " >>. identifier) (pchar ',')) |> between (pchar '(') (pchar ')') 
    let pInputAndOutputNames =  
        pstring "(" 
        >>. pipe3 (sepBy (identifier) ((pstring ", ") <|> pstring ",")) (wsopt .>> str_wsopt ":") (sepBy (identifier) ((pstring ", ") <|> pstring ","))
                (fun inputNames _ outputNames -> (inputNames, outputNames))
        .>> (str_ws ")" .>> wsBeforeEOL)

    let pOutputNames = str_wsopt "(" >>. str_wsopt ":" >>. sepBy (wsopt >>. identifier) (pchar ',') .>> str_wsopt ")"

    let pDeclaredNames = str_wsopt "(" >>. opt (sepBy (wsopt >>. identifier) (pchar ',')) 
                            .>>. opt (str_wsopt ":" >>. (sepBy (str_wsopt " " >>. identifier) (pchar ','))) .>> str_wsopt ")"
                            <!> "Declared Names"

    let declareToLists declareOpt =
        match declareOpt with 
        | None -> [], []
        | Some declare ->
            match declare with
            | None, None -> [], []
            | (Some inputs), None -> inputs, []
            | None, (Some outputs) -> [], outputs
            | (Some inputs), (Some outputs) -> inputs, outputs

    // Multiple inputs.
    let pDefNaryGate = pipe4 (nAryGateMatch) (pint32 .>> ws1) (identifier) (opt pDeclaredNames)
                            (fun gate inputCount name declareNamesOpts -> 
                                let (inputNames, outputNames) = declareToLists declareNamesOpts
                                Define (NaryGate (gate, name, inputCount, inputNames, outputNames))
                            ) <!> "Define Nary Gate"
    // Single input.
    let pDefUnaryGate = pipe3 (unaryGateMatch) (ws1 >>. identifier) (opt pOutputNames)
                            (fun gate name outputOpt ->
                                match outputOpt with
                                | None -> Define (UnaryGate (gate, name, []))
                                | Some outputs -> Define (UnaryGate (gate, name, outputs))
                            ) <!> "Define Unary Gate"
    let pAssign = identifier .>> wsopt .>> str_wsopt "<-" .>>. pexpr //<!> "Assign"

    let pBlock = (many1 (wsopt >>. pAssign .>> str_ws ";" .>> wsBeforeEOL)) |> between (str_ws "{") (pstring "}") <!> "Parse Block"

    let pDefCustomGate = pipe3 (pstring "create" >>. ws1 >>. identifier) (pInputAndOutputNames) (pBlock)
                            (fun name (inputNames, outputNames) exprs -> Define (Custom (name, inputNames, outputNames, exprs)) )
                            <!> "Define Custom Gate"

    let defineGate = (pDefCustomGate <|> pDefNaryGate <|> pDefUnaryGate) .>> wsBeforeEOL <!> "Define Gate"

    let pOneOut = pipe2 identifier (pchar '.' >>. ((pint32 |>> NodeNumber) <|> (identifier |>> NodeName))) (fun i n -> (i, Some n))

    let pOutputType = attempt pOneOut <|> (identifier |>> fun s -> (s, None))

    let pInputNodeType = attempt (ws1 >>. pint32 .>> wsBeforeEOL |>> NodeNumber) <|> (ws1 >>. identifier .>> wsBeforeEOL |>> NodeName)

    let pConToNaryGate = pipe4 (pOutputType) (ws1 >>. pstring ":-") (ws1 >>. identifier) (skipChar ',' >>. pInputNodeType)
                            (fun (name,outChoice) _ target targetNode -> 
                                match outChoice with
                                | None -> Connect (name, target, (OneOut targetNode))
                                | Some n -> Connect (name, target, (ManyOut (targetNode, n)))
                            ) //<!> "Connect To Nary Gate"

    let pConToUnaryGate = pipe3 (pOutputType) (ws1 >>. pstring ":-") (ws1 >>. identifier)
                            (fun (name,outChoice) _ target -> 
                                match outChoice with
                                | None -> Connect (name, target, (OneOut (NodeNumber 1)) ) // 1 is the default value for unary gates.
                                | Some n -> Connect (name, target, (ManyOut (NodeNumber 1, n)))
                            ) //<!> "Connect To Unary Gate"
    
    let connect = (attempt pConToNaryGate <|> pConToUnaryGate) .>> wsBeforeEOL

    let pComment = comment |>> (fun _ -> Comment) <!> "PComment"

    // >>. chains the parsers together like normal while .>> causes the next parser to consume input, but doesn't use the input.

    let statement = (attempt connect <|> defineGate <|> pComment) .>> wsBeforeEOL

    indentedStatementsRef := indentedMany1 statement "statement"

    let statements = many1 statement

    let document = spaces >>. statements .>> spaces .>> eof

    let test str =
        match runParserOnString document (UserState.Create()) "" str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let getProgram str = 
        //printfn "%A" str
        runParserOnString document (UserState.Create()) "" str