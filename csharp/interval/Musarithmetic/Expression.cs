namespace Musarithmetic;
/*
 * pitch + interval = pitch
 * pitch - pitch = interval
 *
 * Patterns:
 *     Pitch: /[a-zA-Z]{1}[bb|b|#|##].*[0-9].* /
 *     Interval: /[mMPda]{1}[0-9].* / (1+) [+ 2x, 3x d, a]
 *         - but Interval a4 vs pitch a4?
 *
 * Examples: 
 * a#4 + P8 = a#5 | Pitch(a#4) + Interval(P8)
 * eb3 + m3 = g3  | Pitch(eb3) + Interval(m3)
 * gb5 - P5 = eb5 | Pitch(gb5) - Interval(P5)
 * gb5 - eb5 = P5 | Pitch(gb5) - Pitch(eb5)

 * == Additions TBD:
 * |interval| = n (absolute chromatic value)
 * ToInterval(n) = interval
 * interval -+ interval = ToInterval(|interval| -+ |interval|)
 * interval /* interval = ToInterval(|interval| -+ |interval|) = interval
 *
 * Examples
 * m2 + m2 = M2 (not d3?)
 * P8 - m3 = M6
 * m2 * 2 = |m2 + m2| = M2
 * P8 / 2 = |P8| / 2 = 6 = d5
 * |m2| = 1
 * |M2| = 2
 * |m2 + m2| = 2
 * 
 */

using System.Collections.Generic;

public class Expression
{
    public static Queue<object> Parse(string expr)
    {

        Queue<object> tokens = new();

        void ThrowNoParseError(string input) 
            => throw new ArgumentException($"Could not parse input '{input}'");

        string[] words = expr.Split(' ');

        foreach (string thisWord in words)
        {
            switch (thisWord)
            {
                case "-":
                case "+":
                    Operator op = new(thisWord);
                    tokens.Enqueue(op);
                    continue;
                default:
                    break;
            }

            if (thisWord.Contains("(") && thisWord.Contains(")"))
            {
                char[] delimiters = ['(', ')'];
                string[] inputs = thisWord.Split(delimiters);

                string prefix, arg;
                if (inputs.Length >= 2 && inputs[1] != "")
                {
                    prefix = inputs[0];
                    arg = inputs[1];
                } else
                    ThrowNoParseError(thisWord);

                switch (prefix)
                {
                    case "Pitch":
                        try {
                            Pitch p = new(arg);
                            tokens.Enqueue(p);
                        }
                        catch { throw; }
                        break;
                    case "Interval":
                        Interval? i;
                        try {
                            i = Interval.FromString(arg);
                            if (i != null)
                                tokens.Enqueue(i);
                            else
                                ThrowNoParseError(thisWord);
                        }
                        catch { throw; }
                        break;
                    default:
                        ThrowNoParseError(thisWord);
                        break;
                }
            } 
            else
                ThrowNoParseError(thisWord);
        }
        return tokens;
    }

    public static void Evaluate(Queue<object> tokens)
    {
        bool IsSubtract(Operator op) =>
            op.operation == Operator.Operation.SUBTRACT;

        void PitchPitchOperation(Operator op, Pitch p1, Pitch p2, Queue<object> tokens)
        {
            if (IsSubtract(op))
            {
                Interval i = new(p2, p1);
                tokens.Enqueue(i);
            }
            else
                throw new ArgumentException("Unknown operation or expression");
        }

        void PitchIntervalOperation(Operator op, Pitch pitch, Interval interval, Queue<object> tokens)
        {
            throw new ArgumentException("I don't know how to add or subtract intervals from pitches yet");
        }


        object[] stream = new object[3];
        while (tokens.Count() >= 3)
        {
            for (int i = 0; i < 3; ++i)
            {
                stream[i] = tokens.Dequeue();
            }

            if (stream[1] is Operator && stream[0] is Pitch)
            {
                if (stream[2] is Pitch)
                    PitchPitchOperation(
                            (Operator)stream[1], 
                            (Pitch)stream[0], 
                            (Pitch)stream[2],
                            tokens);
                else if (stream[2] is Interval)
                    PitchIntervalOperation(
                            (Operator)stream[1],
                            (Pitch)stream[0],
                            (Interval)stream[2],
                            tokens);
                else
                    throw new ArgumentException("Unknown operation or expression");
            }

            /*
            if (stream[1] is Operator 
                    && ((Operator)stream[1]).operation == Operator.Operation.SUBTRACT)
            {
                if (stream[0] is Pitch && stream[2] is Pitch)
                {
                    Interval i = new((Pitch)stream[2], (Pitch)stream[0]);
                    tokens.Enqueue(i);
                    continue;
                }
                else if (stream[0] is Pitch && stream[2] is Interval)
                {
                    // subtract interval from pitch
                    throw new ArgumentException("I don't know how to subtract intervals from pitches yet");
                }
            }
            else
                throw new ArgumentException("Unknown operation or expression");
                */
        }
        if (tokens.Count() == 1)
            Console.WriteLine(tokens.Dequeue());
    }
}

