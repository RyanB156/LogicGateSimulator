using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;
using LogicGateDomain;

namespace LogicGateSim
{
    class Display
    {
        Form form;  // Display for the program.
        List<LogicGate> logicCircuit; // Container for storing all of the gates for the logic circuit.
        List<FormInput> inputFields; // Container for storing all of the input buttons and display logic.
        List<FormOutput> outputFields; // Container for storing all of the output labels and display logic.
        List<IClockable> clockables; // Container for storing all gates that take a clock input.

        Timer timer;

        public event EventHandler DisplayClosed;

        // Position parameters for the input and output fields.
        int inputFieldLeft = 0;
        int inputFieldTop = 2;

        int outputFieldLeft = 200;
        int outputFieldTop = 2;

        int heightBuffer = 5;

        // Create all of the controls required to display the inputs and outputs of the circuit.
        // Add the controls to the form and attach them to the composite "InputField/OutputField" objects for storage.
        public Display(List<LogicGate> circuit)
        {
            logicCircuit = circuit;

            form = new Form()
            {
                Size = new Size(500, 500),
                AutoScroll = true
            };

            form.KeyPreview = true;
            form.FormClosed += Form_FormClosed;
            form.KeyUp += Form_KeyUp;

            timer = new Timer() { Interval = 1000 };
            timer.Tick += Timer_Tick;
            timer.Start();

            inputFields = new List<FormInput>();
            outputFields = new List<FormOutput>();
            clockables = new List<IClockable>();

            // Iterate through the gates in circuit and create the controls for the input and output gates.
            foreach (var gate in logicCircuit)
            {
                switch(gate)
                {
                    case InputGate iGate:
                        FormInput fInput = new FormInput(iGate, inputFieldLeft, inputFieldTop);
                        inputFields.Add(fInput);
                        form.Controls.Add(fInput.textLabel);
                        form.Controls.Add(fInput.button);
                        inputFieldTop += fInput.textLabel.Height + heightBuffer;
                        fInput.InputChanged += FInput_InputChanged;
                        break;
                    case OutputGate oGate:
                        FormOutput fOutput = new FormOutput(oGate, outputFieldLeft, outputFieldTop);
                        outputFields.Add(fOutput);
                        form.Controls.Add(fOutput.textLabel);
                        form.Controls.Add(fOutput.outputLabel);
                        outputFieldTop += fOutput.textLabel.Height + heightBuffer;
                        break;
                    case IClockable cGate:
                        clockables.Add(cGate);
                        break;
                }
            }    
        }

        private void Form_KeyUp(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Q:
                    form.Close(); // Exit the GUI when the user presses "q".
                    break;
            }
        }

        // The form has been closed. Trigger this event.
        private void OnDisplayClosed()
        {
            DisplayClosed?.Invoke(this, new EventArgs());
        }

        // Closing the form in this class triggers the Display's close event which is sent back to Program.cs to close the console application.
        private void Form_FormClosed(object sender, FormClosedEventArgs e)
        {
            OnDisplayClosed();
        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            foreach (IClockable c in clockables)
            {
                c.Tick();
            }
        }

        private void FInput_InputChanged(object sender, EventArgs e)
        {
            FormInput temp = (FormInput)sender;
            //InputGate inputGate = (InputGate)logicCircuit.Find(x => x.GetName() == temp.inputGate.GetName());
            
            foreach (var output in outputFields)
            {
                output.Update();
            }
        }

        public void ShowDisplay()
        {
            form.ShowDialog();
        }
    }

    // Custom class that combines the display and business logic for the input buttons.
    // Each one consists of a label with the input's name and a button that displays the current state and allows the user to alter the state.
    // Allows the user to toggle the state of inputs to the logic circuit.
    class FormInput
    {
        public Label textLabel { get; private set; }
        public Button button { get; private set; }
        public InputGate inputGate { get; private set; }

        public event EventHandler InputChanged;

        public FormInput(InputGate gate, int x, int y)
        {
            inputGate = gate;
            textLabel = new Label()
            {
                Location = new Point(x, y),
                Text = gate.Name,
                BorderStyle = BorderStyle.FixedSingle,
                TextAlign = ContentAlignment.MiddleCenter
            };
            button = new Button()
            {
                Location = new Point(x + textLabel.Width + 2, y),
                Text = BoolAsString(gate.Input),
            };
            button.Click += Button_Click;
        }

        private void Button_Click(object sender, EventArgs e)
        {
            // Toggles the input state and updates the button's text.
            Console.WriteLine("Gate {0} toggled from {1} to {2}", inputGate.Name, inputGate.GetOutput(), !inputGate.GetOutput());

            inputGate.SetInputAndFire(!inputGate.Input);
            button.Text = BoolAsString(inputGate.Input);
            OnInputChanged();
        }

        public void OnInputChanged()
        {
            InputChanged?.Invoke(this, new EventArgs());
        }

        public string BoolAsString(bool b) => $"{b}";
    }

    // Custom class for combining the display and business logic for displaying the output of the logic circuit.
    // Consists of two labels. One for the output's name and the other for the output's state.
    class FormOutput
    {
        OutputGate outputGate;
        public Label textLabel { get; private set; }
        public Label outputLabel { get; private set; }

        public FormOutput(OutputGate gate, int x, int y)
        {
            outputGate = gate;
            outputGate.Activated += OutputGate_Activated;

            textLabel = new Label()
            {
                Location = new Point(x, y),
                Text = gate.Name,
                BorderStyle = BorderStyle.FixedSingle,
                TextAlign = ContentAlignment.MiddleCenter
            };
            outputLabel = new Label()
            {
                Location = new Point(x + textLabel.Width + 2, y),
                BorderStyle = BorderStyle.FixedSingle,
                TextAlign = ContentAlignment.MiddleCenter
            };

            Update();
        }

        private void OutputGate_Activated(object sender, EventArgs e)
        {
            Update();
        }

        public void SetRed() { outputLabel.BackColor = Color.FromArgb(100, 255, 0, 0); }
        public void SetGreen() { outputLabel.BackColor = Color.FromArgb(100, 0, 255, 0); }

        public void Update()
        {
            try
            {
                outputLabel.Text = BoolAsString(outputGate.GetOutput());
                if (outputGate.GetOutput())
                    SetGreen();
                else
                    SetRed();
            }
            catch (GateNotConnectedException)
            {
                outputLabel.Text = "N/A";
            }
            
        }

        public string BoolAsString(bool b) => $"{b}";
    }
}

