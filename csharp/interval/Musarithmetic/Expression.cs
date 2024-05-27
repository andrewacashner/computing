namespace Musarithmetic;

public class Expression
{
    public static void Parse(string expr, out string note1, out string note2)
    {
        string[] words = expr.Split(' ');
        if (words.Length == 3 && words[1] == "-")
        {
            note2 = words[0];
            note1 = words[2];
        }
        else
            throw new ArgumentException($"Could not parse expression '{expr}'");
    }
}

