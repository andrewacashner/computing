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
        object PitchPlusInterval(Pitch pitch, Interval interval, Operator op)
        {
            if (op.IsSubtract())
                interval = interval.Negate();
            
            return (Pitch)pitch.Inc(interval);
        }

        object accumulator = tokens.Dequeue();
        
        while (tokens.Count() >= 2)
        {
            object argA = tokens.Dequeue();
            object argB = tokens.Dequeue();

            Console.WriteLine($"{accumulator.ToString()} {argA.ToString()} {argB.ToString()}");

            switch (accumulator, argA, argB)
            {
                case (Pitch p1, Operator op, Pitch p2):
                    if (op.IsSubtract())
                        accumulator = new Interval((Pitch)p1, (Pitch)p2);
                    break;

                case (Pitch pitch, Operator op, Interval interval):
                   
                    if (op.IsAddOrSubtract())
                        accumulator = PitchPlusInterval(
                                (Pitch)pitch, 
                                (Interval)interval, 
                                (Operator)op);
                    break;

                case (Interval interval, Operator op, Pitch pitch):
                    
                    if (op.IsAdd())
                        accumulator = PitchPlusInterval(
                                (Pitch)pitch, 
                                (Interval)interval,
                                (Operator)op);
                    break;

                default:
                    throw new ArgumentException("Unknown operation or expression");
            }
        }

        Console.WriteLine(accumulator.ToString());
    }
}

