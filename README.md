# LogicGateSimulator
A programming language that allows you to simulate logic circuits. 
You can toggle inputs on or off to see the output of your circuit. 
It supports basic logic gates such as AND, OR, and NOT, as well as Decoders, Multiplexers, Flip-Flops. 
You can also create your own logic gate with any number of inputs and outputs.

See https://github.com/RyanB156/LogicGateSimulator/blob/master/LogicGateSim/Documentation.txt for documentation.

F# code in FCircuitParser is used to parse the program with the FParsec Library and creates logic gate objects as defined in 
LogicGateDomain and links them together.  
The C# Windows forms project in LogicGateSim takes the completed circuit and displays the inputs and outputs on a form where the user can
click to toggle the inputs for the circuit and see the output.  

----Syntax----

<GateType>[input count][(In1Name, In2Name, ..., InNName [: Out1Name, ..., OutNName])] - Initialize gates.
<source gate name>[.<output number> (0 indexed)] :- <target gate name>[, <target gate input choice> (1 indexed)] - Connect gates.
define <customGateTypeName>(In1Name, ..., InNName : Out1Name, ..., OutNName) - Define a custom gate type.
<customGateTypeName>(<customGateInstanceName>) - Create an instance of the custom gate.
{  
    Out1Name <- (Boolean expression using [', ., +, =>, ==, <>]) ; [// comment]  
    ..., OutNName <- ... ;  
}  
[// comment]  
[/* block comment */]  
  
----Sample Program----  
// Negation circuit.  
// Creates an input, output, and a custom gate acting as a NOT gate.  
Input Input1  
//Input Input2  
//Input Input3  
  
define myGate(a : o)  
{  
    o <- a';  
}  
myGate(myGate1)
Output Output1  
  
  
Input1 :- myGate1, a  
myGate1.o :- Output1  
  
----------------------  
  
----Sample Program----  
// Full adder using a custom gate as a decoder (these are also supported by themselves) and n-input or gates.  
Input Input1  
Input Input2  
Input Input3  
  
Or4 SumOr  
Or4 CarryOr  
  
Output Sum  
Output Carry  
  
create myGate(a, b, c : A, B, C, D, E, F, G, H)  
{  
    A <- a'.b'.c';  
    B <- a'.b'.c;  
    C <- a'.b.c';  
    D <- a'.b.c;  
    E <- a.b'.c';  
    F <- a.b'.c;  
    G <- a.b.c';  
    H <- a.b.c;  
}  
  
Input1 :- myGate1, a // Reference inputs to the next gate with <next gate name>, <input name (or number if using the default gates)>  
Input2 :- myGate1, b  
Input3 :- myGate1, c  
  
myGate.B :- SumOr, 1  
myGate.C :- SumOr, 2  
myGate.E :- SumOr, 3  
myGate.H :- SumOr, 4  
  
myGate.D :- CarryOr, 1  
myGate.F :- CarryOr, 2  
myGate.G :- CarryOr, 3  
myGate.H :- CarryOr, 4  
  
SumOr :- Sum  
CarryOr :- Carry  
  
----------------------  
