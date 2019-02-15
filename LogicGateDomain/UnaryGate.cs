using System;
using System.Collections.Generic;

namespace LogicGateDomain
{
    // Base type for unary gates. These can have only one input.
    public abstract class UnaryGate : SingleOutputGate
    {
        public bool Input { get; protected set; } = false;

        public UnaryGate(string name) : base(name, 1, new List<string>()) { }

        public void Reset()
        {
            Input = false;
            output = false;
            IsFilled = false;
        }

        public int GetInputCount()
        {
            return 1;
        }

        public void SendOutput()
        {
            foreach (var connection in ConnectedGates)
            {
                connection.TargetGate.Activate(connection.InputNode, output);
            }
        }

        public abstract void CheckOutput();

        public override void Activate(int inputSide, bool input)
        {
            IsFilled = true;
            Input = input;
            CheckOutput();
        }

    }

// Declaration of the many unary gates, each with its own behavior for sending output based on its input.

    public class OutputGate : UnaryGate
    {
        public event EventHandler Activated;

        public OutputGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            output = Input;
            OnActivated();
        }

        public void OnActivated()
        {
            Activated?.Invoke(this, new EventArgs());
        }
    }

    public class NotGate : UnaryGate
    {
        public NotGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            output = !Input;
            SendOutput();
        }
    }

    public class IdentityGate : UnaryGate
    {
        public IdentityGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            output = Input;
            SendOutput();
        }
    }

    public class TrueGate : UnaryGate
    {
        public TrueGate(string name) : base(name)
        {
            Input = true;
            IsFilled = true;
        }
        public override void CheckOutput()
        {
            output = true;
            SendOutput();
        }
    }

    public class FalseGate : UnaryGate
    {
        public FalseGate(string name) : base(name)
        {
            Input = false;
            IsFilled = true;
        }
        public override void CheckOutput()
        {
            
            output = false;
            SendOutput();
        }
    }

    public class InputGate : UnaryGate
    {
        public InputGate(string name, bool input) : base(name)
        {
            this.Input = input;
            output = input;
            IsFilled = true;
        }

        public void SetInput(bool input)
        {
            this.Input = input;
            output = input;
        }        

        public override void CheckOutput()
        {
            output = Input;
            SendOutput();
        }

        public void Fire()
        {
            output = Input;
            SendOutput();
        }

        public void SetInputAndFire(bool input)
        {
            this.Input = input;
            output = input;
            SendOutput();
        }

    }
}
