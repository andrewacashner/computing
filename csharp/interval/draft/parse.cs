namespace Musarithmetic;

public class Expression
{

    bool IsValidOperator(string input)
    {
        return ["-"].Contains(input);
    }

    bool IsExpression(string w1, string w2, string w3)
    {
        return Pitch.IsValidInput(w1)
            && IsValidOperator(w2)
            && Pitch.IsValidInput(w3);
    }

    public static void Parse(string expr, out string note1, out string note2)
    {
        string[] words = expr.Split(' ');

        Pitch? lastResult = null;
        string[2] next;
        string[3] sample;
        int i = 0;
        int skip = 0;
        while (i < words.Length - 3)
        {
            if (IsExpression(words[i], words[i + 1], words[i + 2]))
            {
                Pitch p1 = new Pitch(words[i]);
                string op = words[i + 1];
                Pitch p2 = new Pitch(words[i + 2]);
                if (op == "-")
                {
                    Interval lastResult = new(p1, p2);
                }
            }
            
            // Look three at a time for pattern [[note] [operator] [note]]
            // If found, evaluate expression and replace
            // Continue using replacement as first note
            if (lastResult == null)
            {
                sample = words[i .. i + 2];
                skip = 3;
            } else
            {
                sample[0] = current;
                sample[1..2] = words[i .. i + 1];
                skip = 2;
            }

            if (IsExpression(sample, "-"))
            {
                lastResult = Evaluate(sample);
                i += skip;
            } else
            {

            }

        }

        if (IsExpression(words, "-"))
        {
            note2 = words[0];
            note1 = words[2];
        }
        else
            throw new ArgumentException($"Could not parse expression '{expr}'");
    }
}

