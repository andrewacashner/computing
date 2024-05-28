public class Operator
{

    public enum Operation { ADD, SUBTRACT, MULTIPLY, DIVIDE };

    public Operation operation;

    public Operator(string op)
    {
        operation = op switch 
        {
            "+" => Operation.ADD,
            "-" => Operation.SUBTRACT,
            "*" => Operation.MULTIPLY,
            "/" => Operation.DIVIDE,
            _ => throw new Exception($"Bad operator input {op}")
        };
    }

    public override string ToString()
    {
        string[] ValidOutputs = ["+", "-", "*", "/"];
        return ValidOutputs[(int)operation];
    }


}


