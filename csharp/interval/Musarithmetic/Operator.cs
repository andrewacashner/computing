public enum Operator { ADD, SUBTRACT, MULTIPLY, DIVIDE };

public static class OperatorHelper
{
    public static Operator ToOperator(this string opStr)
    {
        return opStr switch 
        {
            "+" => Operator.ADD,
            "-" => Operator.SUBTRACT,
            "*" => Operator.MULTIPLY,
            "/" => Operator.DIVIDE,
            _ => throw new Exception($"Bad operator input {opStr}")
        };
    }

    public static string ToSymbol(this Operator op)
    {
        string[] ValidOutputs = ["+", "-", "*", "/"];
        return ValidOutputs[(int)op];
    }


}


