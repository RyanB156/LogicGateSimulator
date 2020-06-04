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
        public new string Type { get; private set; } = "NDecoder";
        public int OutputCount { get; private set; }

        public bool[] InputStates { get; private set; }
        public bool[] AreInputsUpdated { get; private set; }

        public NDecoder(string name, int inputCount, List<string> inputNameList, List<string> outputNameList) 
            : base(name, inputCount, (int)Math.Pow(2, inputCount), inputNameList, outputNameList)
        {
            OutputCount = (int)Math.Pow(2, inputCount);
            InputStates = new bool[inputCount]; // Multiple inputs to keep track of.
            AreInputsUpdated = new bool[inputCount];

            OutputMap = new List<OutputConnection>[OutputCount];
            for (int i = 0; i < OutputCount; i++)
            {
                OutputMap[i] = new List<OutputConnection>();
            }
        }

        public override void Activate(int inputSide, bool input)
        {
            InputStates[inputSide - 1] = input; // Input numbers are 1 indexed.
            AreInputsUpdated[inputSide - 1] = true;

            if (AreInputsUpdated.ToList().TrueForAll(x => x))
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
            for (int i = 0; i < InputStates.Length; i++)
            {
                if (InputStates[i])
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

            for (int i = 0; i < OutputCount; i++)
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
        public new string Type { get; private set; } = "Multiplexer";
        public int ControlCount { get; private set; }

        public bool[] InputStates { get; private set; }
        public bool[] ControlStates { get; private set; }
        public bool[] AreInputsUpdated { get; private set; }
        public bool[] AreControlsUpdated { get; private set; }
        
        // Inputs for a Multiplexer4 given as (control1, control2, input1, input2, input3, input4).
        public Multiplexer(string name, int inputCount, List<string> inputNameList) : base(name, inputCount, inputNameList)
        {
            ControlCount = (int)(Math.Log(inputCount) / Math.Log(2));

            InputStates = new bool[inputCount];
            AreInputsUpdated = new bool[inputCount];
            ControlStates = new bool[ControlCount];
            AreControlsUpdated = new bool[ControlCount];

            ConnectedGates = new List<OutputConnection>();
        }

        public override void Activate(int inputSide, bool input)
        {
            if (inputSide >= 1 && inputSide <= ControlCount)
            {
                ControlStates[inputSide - 1] = input;
                AreControlsUpdated[inputSide - 1] = true;
            }
            else
            {
                inputSide = inputSide - ControlCount;
                InputStates[inputSide - 1] = input; // Input numbers are 1 indexed.
                AreInputsUpdated[inputSide - 1] = true;
            }

            if (AreInputsUpdated.ToList().TrueForAll(x => x) && AreControlsUpdated.ToList().TrueForAll(x => x))
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
            for (int i = 0; i < ControlStates.Length; i++)
            {
                if (ControlStates[i])
                    term += (int)Math.Pow(2, i);
            }
            return term;
        }

        private void SendOutput()
        {
            Output = InputStates[ControlMinTerm()];
            foreach (var connection in ConnectedGates)
            {
                connection.TargetGate.Activate(connection.InputNode, Output);
            }
        }
    }

    // 1 input and 2 outputs. Triggered by a clock.
    public class DFlipFlop : MultipleOutputGate, IClockable
    {
        public new string Type { get; private set; } = "DFlipFlop";
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
        public new string Type { get; private set; } = "TFlipFlop";
        public bool T { get; private set; }
        public bool State { get; private set; } = false;

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
        public new string Type { get; private set; } = "JKFlipFlop";
        public bool J { get; private set; }
        public bool K { get; private set; }
        public bool State { get; private set; } = false;

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
