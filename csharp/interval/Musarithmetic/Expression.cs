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
    public static bool IsExpression(string[] words, string operatorStr)
    {
        return words.Length == 3 
            && words[1] == operatorStr;
    }

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
                if (inputs.Length >= 2)
                {
                    prefix = inputs[0];
                    arg = inputs[1];
                } else
                    ThrowNoParseError(thisWord);

                switch (prefix)
                {
                    case "Pitch":
                        Pitch p = new(arg);
                        tokens.Enqueue(p);
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

    static object? NextToken(Queue<object> tokens)
    {
        object obj;
        Pitch? pitch = null;
        Interval? interval = null;
        Operator? op = null;

        obj = tokens.Dequeue();
        switch (obj)
        {
            case Pitch p:
                pitch = p;
                break;
            case Interval i:
                interval = i;
                break;
            case Operator o:
                op = o;
                break;
            default:
                break;
        }
       
        if (pitch != null)
            return pitch;
        else if (interval != null)
            return interval;
        else if (op != null)
            return op;
        else 
            return null;
    }

    public static void Evaluate(Queue<object> tokens)
    {
        object o1, o2, o3;
        while (tokens.Count() >= 3)
        {
            o1 = tokens.Dequeue();
            o2 = tokens.Dequeue();
            o3 = tokens.Dequeue();

            if (o2 is Operator 
                    && ((Operator)o2).operation == Operator.Operation.SUBTRACT)
            {
                if (o1 is Pitch && o3 is Pitch)
                {
                    Interval i = new((Pitch)o3, (Pitch)o1);
                    tokens.Enqueue(i);
                    continue;
                }
                else if (o1 is Pitch && o3 is Interval)
                {
                    // subtract interval from pitch
                    throw new ArgumentException("I don't know how to subtract intervals from pitches yet");
                }
            }
            else
            {
                throw new ArgumentException("Unknown operation or expression");
            }
        }
        if (tokens.Count() == 1)
        {
            Console.WriteLine(tokens.Dequeue());
        }
    }
}

