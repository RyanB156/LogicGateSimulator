using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LogicGateDomain
{
    public interface IClockable
    {
        void Tick();
    }

    public abstract class LogicGate
    {
        public bool IsFilled { get; protected set; } = false;
        public int InputCount { get; protected set; }
        public string Name { get; protected set; }
        protected bool output;

        protected Dictionary<string, int> InputNameMap;

        public LogicGate(string name, int inputCount, List<string> inputNames)
        {
            InputCount = inputCount;
            Name = name;
            InputNameMap = new Dictionary<string, int>();

            if (inputNames.Count > 0)
            {
                for (int i = 0; i < inputNames.Count; i++)
                {
                    InputNameMap.Add(inputNames[i], i + 1);
                }
            }
        }

        public int GetIndexForInputName(string inputName)
        {
            if (InputNameMap.ContainsKey(inputName))
            {
                return InputNameMap[inputName];
            }
            else
                return -1; // Flag to make communication between the F# and C# code easier.
        }

        public bool GetOutput()
        {
            if (!IsFilled)
                throw new GateNotConnectedException($"Gate {Name} is not connected");
            else
                return output;
        }
        protected void SetOutput(bool output) => this.output = output;

        public virtual void Activate(int inputSide, bool input)
        {
            throw new NotImplementedException(); // All LogicGates have an "Activate" method, but this cannot be abstract because it is implemented 2 classes down.
        }
        
    }

    public class SingleOutputGate : LogicGate
    {
        public List<OutputConnection> ConnectedGates { get; protected set; } // Single output to map other gates to.

        public SingleOutputGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) // TODO: Change this to an actual list.
        {
            ConnectedGates = new List<OutputConnection>();
        }

        public void AddConnection(OutputConnection outputConnection)
        {
            ConnectedGates.Add(outputConnection);
        }
    }

    public class MultipleOutputGate : LogicGate
    {
        public List<OutputConnection>[] OutputMap { get; protected set; } // Multiple outputs to map other gates to.
        protected Dictionary<string, int> OutputNameMap;

        public MultipleOutputGate(string name, int inputCount, int outputCount, List<string> inputNameList, List<string> outputNameList) 
            : base(name, inputCount, inputNameList) // TODO: Change this to an actual list.
        {
            OutputMap = new List<OutputConnection>[outputCount];
            OutputNameMap = new Dictionary<string, int>();

            if (outputNameList.Count > 0 && outputNameList.Count == outputCount)
            {
                for (int i = 0; i < outputNameList.Count; i++)
                {
                    OutputMap[i] = new List<OutputConnection>();
                    OutputNameMap.Add(outputNameList[i], i + 1);
                }
            }
        }
        
        public virtual void AddConnectionTo(OutputConnection outputConnection, int output)
        {
            OutputMap[output].Add(outputConnection); // outputs 0 indexed.
        }

        public int GetIndexForOutputName(string inputName)
        {
            if (OutputNameMap.ContainsKey(inputName))
            {
                return OutputNameMap[inputName];
            }
            else
                return -1; // Flag to make communication between the F# and C# code easier.
        }
    }

    public class BoolArrayComparer : IEqualityComparer<bool[]>
    {
        public bool Equals(bool[] x, bool[] y)
        {
            if (x.Length != y.Length)
                return false;
            for (int i = 0; i < x.Length; i++)
            {
                if (x[i] != y[i])
                {
                    return false;
                }
            }
            return true;
        }
        
        public int GetHashCode(bool[] bools)
        {
            unchecked
            {
                int hash = 17;
                for (int index = 0; index < bools.Length; index++)
                {
                    hash = hash * 23 + bools[index].GetHashCode();
                }
                return hash;
            }
        }
    }

    // Gates can 1:M outputs, so cover all cases.
    // Create a custom gate with different numbers of inputs and outputs.
    // "outputFunctions" acts as the truth table for the gate. This gets the output based
    public class CustomGate : MultipleOutputGate
    {
        private bool[] outputs; // bool value for each output determined by the activation inputs and the truth tables.
        private int outputCount;
        private Dictionary<bool[], bool>[] outputFunctions;
        private bool[] inputs;
        private bool[] areInputsUpdated;

        public CustomGate(string name, int inputCount, int outputCount, List<string> inputNameList, 
            List<string> outputNameList, Dictionary<bool[], bool>[] outputFunctions)
            : base(name, inputCount, outputCount, inputNameList, outputNameList)
        {
            outputs = new bool[outputCount];
            this.outputCount = outputCount;
            inputs = new bool[inputCount];
            areInputsUpdated = new bool[inputCount];
            this.outputFunctions = outputFunctions;
        }

        // Update outputs based on the truth tables in "outputFunctions".
        private void UpdateOutputs()
        {
            for (int i = 0; i < outputFunctions.Length; i++)
            {
                bool outputState = outputFunctions[i][inputs]; // Could throw an exception...
                outputs[i] = outputState;
            }
        }

        private void SendOutput()
        {
            for (int i = 0; i < outputCount; i++)
            {
                foreach (var connection in OutputMap[i])
                {
                    connection.TargetGate.Activate(connection.InputNode, outputs[i]); // Throws a stack overflow exception.
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
