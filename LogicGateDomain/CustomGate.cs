using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LogicGateDomain
{
    // Gates can 1:M outputs, so cover all cases.
    // Create a custom gate with different numbers of inputs and outputs.
    // "outputFunctions" acts as the truth table for the gate. This gets the output based on the input values.
    public class CustomGate : MultipleOutputGate
    {
        public new string Type { get; private set; } = "CustomGate";
        public bool[] Outputs { get; private set; } // bool value for each output determined by the activation inputs and the truth tables.
        public int OutputCount { get; private set; }
        public Dictionary<int, bool>[] OutputFunctions { get; private set; }
        public bool[] Inputs { get; private set; }
        public bool[] AreInputsUpdated { get; private set; }

        public CustomGate(string name, int inputCount, int outputCount, List<string> inputNameList,
            List<string> outputNameList, Dictionary<int, bool>[] outputFunctions)
            : base(name, inputCount, outputCount, inputNameList, outputNameList)
        {
            Outputs = new bool[outputCount];
            OutputCount = outputCount;
            Inputs = new bool[inputCount];
            AreInputsUpdated = new bool[inputCount];
            OutputFunctions = outputFunctions;
        }

        public static int MintermDigit(bool[] arr)
        {
            int ret = 0;
            for (int i = 0; i < arr.Length; i++)
            {
                ret *= 2;
                ret += arr[i] ? 1 : 0;
            }
            return ret;
        }

        // Update outputs based on the truth tables in "outputFunctions".
        private void UpdateOutputs()
        {
            for (int i = 0; i < OutputFunctions.Length; i++)
            {
                Outputs[i] = OutputFunctions[i][MintermDigit(Inputs)];
            }
        }

        private void SendOutput()
        {
            for (int i = 0; i < OutputCount; i++)
            {
                foreach (var connection in OutputMap[i])
                {
                    connection.TargetGate.Activate(connection.InputNode, Outputs[i]);
                }
            }
        }

        public override void Activate(int inputSide, bool input)
        {
            Inputs[inputSide - 1] = input;
            AreInputsUpdated[inputSide - 1] = true;

            // If all of the inputs have been activated update the outputs and send output.
            if (AreInputsUpdated.ToList().TrueForAll(x => x))
            {
                IsFilled = true;
                UpdateOutputs();
                SendOutput();
            }
        }
    }
}
