using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LogicGateDomain
{

    // N inputs and 2^N outputs.
    public class NDecoder : MultipleOutputGate
    {
        private int outputCount;

        private bool[] inputStates;
        private bool[] areInputsUpdated;

        public NDecoder(string name, int inputCount, List<string> inputNameList, List<string> outputNameList) 
            : base(name, inputCount, (int)Math.Pow(2, inputCount), inputNameList, outputNameList)
        {
            outputCount = (int)Math.Pow(2, inputCount);
            inputStates = new bool[inputCount]; // Multiple inputs to keep track of.
            areInputsUpdated = new bool[inputCount];

            OutputMap = new List<OutputConnection>[outputCount];
            for (int i = 0; i < outputCount; i++)
            {
                OutputMap[i] = new List<OutputConnection>();
            }
        }

        public override void Activate(int inputSide, bool input)
        {
            inputStates[inputSide - 1] = input; // Input numbers are 1 indexed.
            areInputsUpdated[inputSide - 1] = true;

            if (areInputsUpdated.ToList().TrueForAll(x => x))
            {
                IsFilled = true;
                SendOutput();
            }
        }

        // Reads the state of the inputs and returns the decimal value that corresponds to the BCD minterm of the inputs.
        // 000 -> 0, 011 -> 3, 110 -> 6, ...
        private int InputMinTerm()
        {
            int term = 0;
            for (int i = 0; i < inputStates.Length; i++)
            {
                if (inputStates[i])
                    term += (int)Math.Pow(2, i);
            }
            return term;
        }

        private void SendOutput()
        {
            int outputChoice = InputMinTerm();
            Console.WriteLine(outputChoice);
            bool output;
            // Activate all gates connected to all outputs of the decoder.

            for (int i = 0; i < outputCount; i++)
            {
                output = i == outputChoice; // If the output is the active input based on the input state, that output is active. Otherwise, it is false.

                foreach (var connection in OutputMap[i])
                {
                    connection.TargetGate.Activate(connection.InputNode, output); // Throws a stack overflow exception.
                }    
            }
        }
    }

    // Multiple inputs and 1 output.
    public class Multiplexer : SingleOutputGate
    {
        private int controlCount;

        private bool[] inputStates;
        private bool[] controlStates;
        private bool[] areInputsUpdated;
        private bool[] areControlsUpdated;
        
        // Inputs for a Multiplexer4 given as (control1, control2, input1, input2, input3, input4).
        public Multiplexer(string name, int inputCount, List<string> inputNameList) : base(name, inputCount, inputNameList)
        {
            controlCount = (int)(Math.Log(inputCount) / Math.Log(2));

            inputStates = new bool[inputCount];
            areInputsUpdated = new bool[inputCount];
            controlStates = new bool[controlCount];
            areControlsUpdated = new bool[controlCount];

            ConnectedGates = new List<OutputConnection>();
        }

        public override void Activate(int inputSide, bool input)
        {
            if (inputSide >= 1 && inputSide <= controlCount)
            {
                controlStates[inputSide - 1] = input;
                areControlsUpdated[inputSide - 1] = true;
            }
            else
            {
                inputSide = inputSide - controlCount;
                inputStates[inputSide - 1] = input; // Input numbers are 1 indexed.
                areInputsUpdated[inputSide - 1] = true;
            }

            if (areInputsUpdated.ToList().TrueForAll(x => x) && areControlsUpdated.ToList().TrueForAll(x => x))
            {
                IsFilled = true;
                SendOutput();
            }
        }

        // Reads the state of the inputs and returns the decimal value that corresponds to the BCD minterm of the inputs.
        // 000 -> 0, 011 -> 3, 110 -> 6, ...
        private int ControlMinTerm()
        {
            int term = 0;
            for (int i = 0; i < controlStates.Length; i++)
            {
                if (controlStates[i])
                    term += (int)Math.Pow(2, i);
            }
            return term;
        }

        private void SendOutput()
        {
            output = inputStates[ControlMinTerm()];
            foreach (var connection in ConnectedGates)
            {
                connection.TargetGate.Activate(connection.InputNode, output);
            }
        }
    }

    // 1 input and 2 outputs. Triggered by a clock.
    public class DFlipFlop : MultipleOutputGate, IClockable
    {
        bool D;

        public DFlipFlop(string name, List<string> outputNameList)
            : base(name, 1, 2, new List<string>(), outputNameList)
        {
            OutputMap = new List<OutputConnection>[2]; // Two Outputs. Q and Q'.
            OutputMap[0] = new List<OutputConnection>();
            OutputMap[1] = new List<OutputConnection>();
        }

        public override void Activate(int inputSide, bool input)
        {
            D = input;
            IsFilled = true;
        }

        private void SendOutput()
        {
            // Qt+1 = D.
            foreach (var c in OutputMap[0])
                c.TargetGate.Activate(c.InputNode, D);

            foreach (var c in OutputMap[1])
                c.TargetGate.Activate(c.InputNode, !D);
        }

        // Called from the form's Timer. This causes the Flip Flop to change to the next state.
        // The flip flop only gives its output with the timer. This should prevent an infinite loop.
        public void Tick()
        {
            SendOutput();
        }
    }

    public class TFlipFlop : MultipleOutputGate, IClockable
    {
        bool T;
        bool State = false;

        public TFlipFlop(string name, List<string> outputNameList) 
            : base(name, 1, 2, new List<string>(), outputNameList)
        {
            OutputMap = new List<OutputConnection>[2]; // Two Outputs. Q and Q'.
            OutputMap[0] = new List<OutputConnection>();
            OutputMap[1] = new List<OutputConnection>();
        }

        public override void Activate(int inputSide, bool input)
        {
            T = input;
        }

        private void SendOutput()
        {
            State ^= T; // Qt+1 = Q^T.

            foreach (var c in OutputMap[0])
                c.TargetGate.Activate(c.InputNode, State);

            foreach (var c in OutputMap[1])
                c.TargetGate.Activate(c.InputNode, !State);
        }

        public void Tick()
        {
            SendOutput();
        }
    }

    public class JKFlipFlop : MultipleOutputGate, IClockable
    {
        bool J;
        bool K;
        bool State = false;

        public JKFlipFlop(string name, int inputCount, List<string> inputNameList, List<string> outputNameList) 
            : base(name, inputCount, 2, inputNameList, outputNameList)
        {
            OutputMap = new List<OutputConnection>[2]; // Two Outputs. Q and Q'.
            OutputMap[0] = new List<OutputConnection>();
            OutputMap[1] = new List<OutputConnection>();
        }

        public override void Activate(int inputSide, bool input)
        {
            switch (inputSide)
            {
                case 0:
                    J = input;
                    break;
                case 1:
                    K = input;
                    break;
            }
        }

        private void SendOutput()
        {
            State = (!K && State) || (J && !State); // Qt+1 = K'Q + JQ'.

            foreach (var c in OutputMap[0])
                c.TargetGate.Activate(c.InputNode, State);

            foreach (var c in OutputMap[1])
                c.TargetGate.Activate(c.InputNode, !State);
        }

        public void Tick()
        {
            SendOutput();
        }
    }
}
