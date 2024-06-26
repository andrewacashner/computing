namespace Musarithmetic;

using System.Collections.Generic;

public class Expression
{
    public static Queue<object> Parse(string expr)
    {
        void ThrowNoParseError(string input) 
            => throw new ArgumentException($"Could not parse input '{input}'");

        bool IsValidOperandInput(string inputStr)
        {
            return (inputStr.StartsWith("Interval(") 
                    || inputStr.StartsWith("Pitch("))
                && inputStr.EndsWith(")");
        }

        void EnqueueNewOperator(string operatorStr, Queue<object> tokens)
        {
            try
            {
                Operator op = new Operator(operatorStr);
                tokens.Enqueue(op);
            }
            catch { throw; }
        }

        void EnqueueNewPitch(string pitchStr, Queue<object> tokens)
        {
            try
            {
                Pitch p = new(pitchStr);
                tokens.Enqueue(p);
            }
            catch { throw; }
        }

        void EnqueueNewInterval(string intervalStr, Queue<object> tokens)
        {
            Interval i;
            try 
            {
                i = Interval.FromString(intervalStr);
                tokens.Enqueue(i);
            }
            catch { throw; }
        }

        Queue<object> tokens = new();

        string[] words = expr.Split(' ');
        
        foreach (string thisWord in words)
        {
            switch (thisWord)
            {
                case "-":
                case "+":
                    EnqueueNewOperator(thisWord, tokens);
                    continue;

                default:
                    break;
            }

            if (IsValidOperandInput(thisWord))
            {
                string[] inputs = thisWord.Split(['(', ')'],
                        StringSplitOptions.RemoveEmptyEntries);

                switch (inputs)
                {
                    case ["Pitch", string arg]:
                        EnqueueNewPitch(arg, tokens);
                        break;

                    case ["Interval", string arg]:
                        EnqueueNewInterval(arg, tokens);
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

