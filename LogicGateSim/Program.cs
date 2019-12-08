using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LogicGateDomain;
using FCircuitParser;
using static FCircuitParser.FParser.CircuitTypes;

// Ryan Bressette
// Started 4 May 2018
// Last Updated: 11 November 2018

/* Changelog:
 * Name conflict checking added to the parser. 11 November 2018.
 * Decoder and Multiplexer added 21 November 2018
 * Interface based grouping of the logic gates changed to a more efficient class heirarchy 5 December 2018.
 * Parser updated to better reflect the [1/many input] * [1/many output] pattern
 * Flip Flops added (D, T, and JK). 7 December 2018.
 * Added support for named inputs for all gates for use in the interpreter stage. 7 December 2018.
 * Added named outputs for some gates. 11 December 2018.
 * Added custom logic gates that can be defined using logical expressions. 11 December 2018.
 * Simplified code for computing the output NaryGates. Replaced for loop with list reduce. 7 December 2019.
 */

/* Todo:
 * Add more error checking and syntax errors to the parser.
 * 
 * Add check for multiple gates connected to the same input on another gate. This will create a short circuit in real life.
 * Extensive bug checking and fixing. Make sure everything works for all valid inputs and fails with helpful error messages otherwise.
 * Add named inputs and input name lookups. Possibly make all names mandatory for the multiplexer because of its controls and separate inputs.
 * 
 * Multiplexer control minterms are reversed. 00 :- i0, 01 :- i2, 10 :- i1, 11 :- i3.
 */


namespace LogicGateSim
{
    public class Program
    {
        static void Main(string[] args)
        {
            int testCount = 4;
            Random rng = new Random();

            bool inConsole = false;
            string path;

            List<LogicGate> circuit;
            List<InputGate> inputGates;
            List<UnaryGate> trueFalseGates;

            if (args.Length == 1)
            {
                inConsole = true;
                path = args[0];
            }
            else if (args.Length == 2)
            {
                inConsole = true;
                path = args[0];
                int.TryParse(args[1], out testCount);
            }
            else
            {
                path = @"C:\Users\bress\Documents\Visual Studio 2017\Projects\C#\logicgatesim\logicgatesim\testcircuit.txt";
            } 

            circuit = new List<LogicGate>();


            // Send path to BuildCircuit. BuildCircuit calls the FSharp parser and gets a string representation of the circuit that is easier to work with.
            try
            {
                circuit = BuildCircuit(path);
            }
            catch (Exception ex) // Catch any exceptions from the F# section. Prints its custom error message.
            {
                Console.WriteLine(ex.Message);
            }
            
            
            int inputCount = 0;
            inputGates = new List<InputGate>(); // List of input gates to be fired with the generated test inputs lower down.
            trueFalseGates = new List<UnaryGate>();

            foreach (LogicGate gate in circuit)
            {
                switch (gate)
                {
                    case InputGate g:
                        inputGates.Add(g);
                        inputCount++;
                        break;
                    case TrueGate t:
                        trueFalseGates.Add(t);
                        break;
                    case FalseGate f:
                        trueFalseGates.Add(f);
                        break;
                    case IClockable c: // Trigger items that will be controlled by the clock when the form loads after these test cases.
                        c.Tick();
                        break;
                    default:
                        continue;
                }
            }

            // Get all test cases from F# function and convert to a C# friendly format.
            var fTestCases = FCircuitParser.BinaryPermutations.limitedBinaryList(inputCount, testCount);
            List<List<bool>> testCases = new List<List<bool>>(); // Convert F# list to C# list.

            if (fTestCases.Length > 1)
            {
                // Convert F# List<List<bool>> to C# List<List<bool>>.
                foreach (var testList in fTestCases)
                {
                    List<bool> boolList = new List<bool>();
                    foreach (var boole in testList)
                    {
                        boolList.Add(boole);
                    }
                    testCases.Add(boolList);
                }

                // Have to use nested for loops because foreach does not like List<List<T>>.
                for (int i = 0; i < testCases.Count; i++)
                { // Beginning of test loop.
                    for (int j = 0; j < inputGates.Count; j++)
                    {
                        // Set input to the InputGates based on the test cases and fire all of the inputs.
                        inputGates[j].SetInput(testCases[i][j]);
                        inputGates[j].Fire();
                    }

                    // Fire the default output gates (true / false).
                    foreach (var defaultGate in trueFalseGates)
                    {
                        defaultGate.CheckOutput();
                    }

                    // Check for any gates that aren't connected.
                    foreach (var gate in circuit)
                    {
                        if (!gate.IsFilled)
                        {
                            Console.WriteLine($"Gate {gate.Name} is not connected");
                        }
                    }

                    // Check Output.
                    Console.WriteLine("--Input--");
                    foreach (var gate in circuit)
                    {
                        switch (gate)
                        {
                            case InputGate input:
                                Console.WriteLine("{0}: {1}", input.Name, input.Input);
                                break;
                            default:
                                continue;
                        }
                    }

                    Console.WriteLine("--Output--");
                    foreach (var gate in circuit)
                    {
                        try
                        {
                            switch (gate)
                            {
                                case OutputGate output:
                                    Console.WriteLine("{0}: {1}", output.Name, output.GetOutput());
                                    break;
                                default:
                                    continue;
                            }
                        }

                        catch (GateNotConnectedException ex)
                        {
                            Console.WriteLine(ex.Message);
                        }
                    }

                    Console.WriteLine();
                } // End of test loop.
            }



            //--Display--//

            Display display = new Display(circuit);
            display.DisplayClosed += Display_DisplayClosed;
            display.ShowDisplay();

            if (! inConsole)
                Console.ReadKey();
        }

        private static void Display_DisplayClosed(object sender, EventArgs e)
        {
            Environment.Exit(0);
        }

        /// <summary>
        /// Calls FSharp parser to parse the circuit. Fills the circuit with the logic gate objects according to the code file.
        /// </summary>
        /// <param name="path">Path to the Circuit source file</param>
        /// <returns></returns>
        static List<LogicGate> BuildCircuit(string path)
        {
            List<Statement> commandList = Parser.getProgram(path);
            
            List<LogicGate> circuit = FCircuitParser.Parser.getCircuit(commandList);  

            return circuit;
        }
    }
}


