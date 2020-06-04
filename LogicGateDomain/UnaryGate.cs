using System;
using System.Collections.Generic;

namespace LogicGateDomain
{
    // Base type for unary gates. These can have only one input.
    public abstract class UnaryGate : SingleOutputGate
    {
        public new string Type { get; private set; } = "UnaryGate";
        public bool Input { get; protected set; } = false;

        public UnaryGate(string name) : base(name, 1, new List<string>()) { }

        public void Reset()
        {
            Input = false;
            Output = false;
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
                connection.TargetGate.Activate(connection.InputNode, Output);
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

// Declaration of the many unary gates, each with its own behavior for sending Output based on its input.

    public class OutputGate : UnaryGate
    {
        public new string Type { get; private set; } = "OutputGate";
        public event EventHandler Activated;

        public OutputGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            Output = Input;
            OnActivated();
        }

        public void OnActivated()
        {
            Activated?.Invoke(this, new EventArgs());
        }
    }

    public class NotGate : UnaryGate
    {
        public new string Type { get; private set; } = "NotGate";
        public NotGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            Output = !Input;
            SendOutput();
        }
    }

    public class IdentityGate : UnaryGate
    {
        public new string Type { get; private set; } = "IdentityGate";
        public IdentityGate(string name) : base(name) { }
        public override void CheckOutput()
        {
            Output = Input;
            SendOutput();
        }
    }

    public class TrueGate : UnaryGate
    {
        public new string Type { get; private set; } = "TrueGate";
        public TrueGate(string name) : base(name)
        {
            Input = true;
            IsFilled = true;
        }
        public override void CheckOutput()
        {
            Output = true;
            SendOutput();
        }
    }

    public class FalseGate : UnaryGate
    {
        public new string Type { get; private set; } = "FalseGate";
        public FalseGate(string name) : base(name)
        {
            Input = false;
            IsFilled = true;
        }
        public override void CheckOutput()
        {
            
            Output = false;
            SendOutput();
        }
    }

    public class InputGate : UnaryGate
    {
        public new string Type { get; private set; } = "InputGate";
        public InputGate(string name, bool input) : base(name)
        {
            this.Input = input;
            Output = input;
            IsFilled = true;
        }

        public void SetInput(bool input)
        {
            this.Input = input;
            Output = input;
        }        

        public override void CheckOutput()
        {
            Output = Input;
            SendOutput();
        }

        public void Fire()
        {
            Output = Input;
            SendOutput();
        }

        public void SetInputAndFire(bool input)
        {
            this.Input = input;
            Output = input;
            SendOutput();
        }
    }

    // Clock will be paired with a timer to generate the clock signal.
    // Needs to output false, unless the timer ticks, then it will output true.
    // Relearn how the input gates work because the timer may have to activate Clock gates in a similar manner.
    public class Clock : UnaryGate
    {
        public Clock(string name, int interval) : base(name)
        {

        }

        public override void CheckOutput()
        {
            throw new NotImplementedException();
        }

        
    }
}
