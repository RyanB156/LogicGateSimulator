namespace LogicGateDomain
{
    /* "Circuit" class to hold all of the logic gates
         * List of all the gates and a queue of all of the gates that have not been "filled" yet
         * While loop over the queue to update and send the full output
         */

    public class OutputConnection
    {
        public string Type { get; private set; } = "OutputConnection";
        public LogicGate TargetGate { get; private set; }
        public int InputNode { get; private set; }

        public OutputConnection(LogicGate targetGate, int inputNode)
        {
            TargetGate = targetGate;
            InputNode = inputNode;
        }
    }
}
