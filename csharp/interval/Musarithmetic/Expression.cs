namespace Musarithmetic;

public class Expression
{
    public static Queue<object> Parse(string expr)
    {
        bool IsValidOperandInput(string s)
            => (s.StartsWith("Interval(") || s.StartsWith("Pitch("))
                && s.EndsWith(")");

        ValueTuple<string, string> ParseToken(string word)
        {
            string[] inputs = word.Split(['(', ')'],
                    StringSplitOptions.RemoveEmptyEntries);

            if (inputs.Length == 2)
                return (inputs.ElementAt(0), inputs.ElementAt(1));
            else
                throw new ArgumentException(
                        $"Problem parsing input token {word}");
        }

        Queue<object> tokens = new();

        string[] words = expr.Split(' ');
        
        foreach (string thisWord in words)
        {
            try 
            {
                object? new_token = null;

                if (thisWord is "-" or "+")
                {
                    new_token = new Operator(thisWord);
                } 
                else if (IsValidOperandInput(thisWord))
                {
                    var (token_type, arg) = ParseToken(thisWord);

                    new_token = token_type switch
                    {
                        "Pitch"     => new Pitch(arg),
                        "Interval"  => Interval.FromString(arg),
                        _           => throw new ArgumentException(
                                        $"Unrecognized input {thisWord}")
                    };
                }

                if (new_token != null)
                    tokens.Enqueue(new_token);
                else
                    throw new ArgumentException(
                            $"Could not parse input {thisWord}");
            }
            catch (Exception e) { 
                throw new ArgumentException($"Input error: {e.Message}");
            }
        }
        return tokens;
    }

    public static void Evaluate(Queue<object> tokens)
    {
        object accumulator = tokens.Dequeue();
     
        Interval SubtractPitches(Pitch p1, Pitch p2, Operator op) => 
            op.Meaning switch
            {
                Operator.kOperator.SUBTRACT => p1 - p2,
                _ => throw new ArgumentException($"Cannot process operation {op}")
            };

        Pitch PitchPlusInterval(Pitch pitch, Interval interval, Operator op) => 
            op.Meaning switch
            {
                Operator.kOperator.ADD => pitch + interval,
                Operator.kOperator.SUBTRACT => pitch - interval,
                _ => throw new ArgumentException($"Cannot process operation {op}")
            };

        while (tokens.Count() >= 2)
        {
            object argA = tokens.Dequeue();
            object argB = tokens.Dequeue();

            accumulator = (accumulator, argA, argB) switch
            {
                (Pitch p1, Operator o, Pitch p2) =>
                    SubtractPitches((Pitch)p1, (Pitch)p2, (Operator)o),

                (Pitch p, Operator o, Interval i) =>
                    PitchPlusInterval((Pitch)p, (Interval)i, (Operator)o),

                (Interval i, Operator o, Pitch p) =>
                    PitchPlusInterval((Pitch)p, (Interval)i, (Operator)o),
                
                _ => throw new ArgumentException("Unknown expression")
            };
        }

        Console.WriteLine(accumulator.ToString());
    }
}

