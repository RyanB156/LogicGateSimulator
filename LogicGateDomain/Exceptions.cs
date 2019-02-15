using System;
using System.Runtime.Serialization;

namespace LogicGateDomain
{
    [Serializable]
    public class GateNotConnectedException : Exception
    {
        public GateNotConnectedException()
        {
        }

        public GateNotConnectedException(string message) : base(message)
        {
        }

        public GateNotConnectedException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected GateNotConnectedException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }

    public class GateNotDefinedException : Exception
    {
        public GateNotDefinedException()
        {
        }

        public GateNotDefinedException(string message) : base(message)
        {
        }

        public GateNotDefinedException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected GateNotDefinedException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }

    public class ParserErrorException : Exception
    {
        public ParserErrorException()
        {
        }

        public ParserErrorException(string message) : base(message)
        {
        }

        public ParserErrorException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected ParserErrorException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }

    public class InvalidInputCountException : Exception
    {
        public InvalidInputCountException()
        {
        }

        public InvalidInputCountException(string message) : base(message)
        {
        }

        public InvalidInputCountException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected InvalidInputCountException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }

    public class ConnectionTypeException : Exception
    {
        public ConnectionTypeException()
        {
        }

        public ConnectionTypeException(string message) : base(message)
        {
        }

        public ConnectionTypeException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected ConnectionTypeException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }
}