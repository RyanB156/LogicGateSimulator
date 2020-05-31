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
        private bool[] outputs; // bool value for each output determined by the activation inputs and the truth tables.
        private int outputCount;
        private Dictionary<int, bool>[] outputFunctions;
        private bool[] inputs;
        private bool[] areInputsUpdated;

        public CustomGate(string name, int inputCount, int outputCount, List<string> inputNameList,
            List<string> outputNameList, Dictionary<int, bool>[] outputFunctions)
            : base(name, inputCount, outputCount, inputNameList, outputNameList)
        {
            outputs = new bool[outputCount];
            this.outputCount = outputCount;
            inputs = new bool[inputCount];
            areInputsUpdated = new bool[inputCount];
            this.outputFunctions = outputFunctions;
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
            for (int i = 0; i < outputFunctions.Length; i++)
            {
                outputs[i] = outputFunctions[i][MintermDigit(inputs)];
            }
        }

        private void SendOutput()
        {
            for (int i = 0; i < outputCount; i++)
            {
                foreach (var connection in OutputMap[i])
                {
                    connection.TargetGate.Activate(connection.InputNode, outputs[i]);
                }
            }
        }

        public override void Activate(int inputSide, bool input)
        {
            inputs[inputSide - 1] = input;
            areInputsUpdated[inputSide - 1] = true;

            // If all of the inputs have been activated update the outputs and send output.
            if (areInputsUpdated.ToList().TrueForAll(x => x))
            {
                IsFilled = true;
                UpdateOutputs();
                SendOutput();
            }
        }
    }
}
