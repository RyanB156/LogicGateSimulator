﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LogicGateDomain
{

    // Create all logic gates without connections then run through a graph and connect all of the gates

    public abstract class NaryGate : SingleOutputGate
    {
        public new string Type { get; private set; } = "NaryGate";
        protected bool[] inputStates;
        protected bool[] areInputsUpdated;
        // List or Array? Valid connections for an N input gate could be determined in the parser step.
        // Add extra validation here? with an exception with a popup?

        // Gate requires both inputs to be set to send an output.

        protected string name;

        public NaryGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList)
        {
            inputStates = new bool[inputCount]; // Store the input states.
            areInputsUpdated = new bool[inputCount]; // Check if all inputs have been updated.
            this.name = name;
        }

        public string GetName() => name;

        public void Reset()
        {
            // Reset the state of the inputs.
            for (int i = 0; i < inputStates.Length; i++)
                inputStates[i] = false;
            // Reset the update state of the inputs.
            for (int i = 0; i < inputStates.Length; i++)
                areInputsUpdated[i] = false;

            Output = false;
            IsFilled = false;
        }

        // Checks state of input gates and gives either a true or false value. Triggers the activation of its connected logic gates.
        // Specific to each logic gate.
        public abstract void CheckOutput();

        public void SendOutput()
        {
            foreach (var connection in ConnectedGates)
            {
                connection.TargetGate.Activate(connection.InputNode, Output);
            }
        }

        // Update either inputs with a true or false value.
        public override void Activate(int inputSide, bool input)
        {
            // N input behavior
            inputStates[inputSide - 1] = input; // input nodes are given as 1, 2, 3, 4, ...; while index are 0, 1, 2, 3, ...
            areInputsUpdated[inputSide - 1] = true;

            if (areInputsUpdated.ToList().TrueForAll(x => x))
            {
                IsFilled = true;
                CheckOutput();
            }
        }
    }

    public class AndGate : NaryGate
    {
        public new string Type { get; private set; } = "AndGate";
        public AndGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }
        public override void CheckOutput()
        {
            Output = inputStates.Aggregate((a, b) => a && b);   
            SendOutput();
        }
    }

    public class OrGate : NaryGate
    {
        public new string Type { get; private set; } = "OrGate";
        public OrGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }
        public override void CheckOutput()
        {
            Output = inputStates.Aggregate((a, b) => a || b);
            SendOutput();
        }
    }

    // DifferentGate
    public class XorGate : NaryGate
    {
        public new string Type { get; private set; } = "XorGate";
        public XorGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }

        public override void CheckOutput()
        {
            Output = inputStates.Aggregate((a, b) => a ^ b);
            // output = AInput ^ BInput;
            SendOutput();
        }
    }

    // EqualGate
    public class XnorGate : NaryGate
    {
        public new string Type { get; private set; } = "XnoreGate";
        public XnorGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }
        public override void CheckOutput()
        {
            Output = !inputStates.Aggregate((a, b) => a ^ b);
            //output = AInput == BInput;
            SendOutput();
        }
    }

    public class NandGate : NaryGate
    {
        public new string Type { get; private set; } = "NandGate";
        public NandGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }
        public override void CheckOutput()
        {
            Output = !inputStates.Aggregate((a, b) => a && b);
            //output = !(AInput && BInput);
            SendOutput();
        }
    }

    public class NorGate : NaryGate
    {
        public new string Type { get; private set; } = "NorGate";
        public NorGate(string name, int inputCount, List<string> nameList) : base(name, inputCount, nameList) { }
        public override void CheckOutput()
        {
            Output = !inputStates.Aggregate((a, b) => a || b);
            //output = !(AInput || BInput);
            SendOutput();
        }
    }
}