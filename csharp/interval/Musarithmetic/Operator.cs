namespace Musarithmetic;

public class Operator
{
    public enum kOperator { ADD, SUBTRACT, MULTIPLY, DIVIDE };

    kOperator op;

    public kOperator Meaning { get => op; }
    int Value { get => (int)op; }

    public Operator(string opStr)
    {
        op = opStr switch 
        {
            "+" => kOperator.ADD,
            "-" => kOperator.SUBTRACT,
            "*" => kOperator.MULTIPLY,
            "/" => kOperator.DIVIDE,
            _ => throw new Exception($"Bad operator input {opStr}")
        };
    }

    public override string ToString() =>
        (new [] {"+", "-", "*", "/"}).ElementAt(this.Value);

    public bool IsAdd()           => op == kOperator.ADD;
    public bool IsSubtract()      => op == kOperator.SUBTRACT;
    public bool IsAddOrSubtract() => IsAdd() || IsSubtract();
}


