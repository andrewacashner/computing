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
                Operator op = OperatorHelper.FromString(operatorStr);
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
        void PitchDiff(Pitch p1, Pitch p2, Queue<object> tokens)
        {
            Interval i = new(p2, p1);
            tokens.Enqueue(i);
        }

        void PitchInc(Operator op, Pitch pitch, Interval interval, Queue<object> tokens)
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

            switch (stream)
            {
                case [Pitch p1, Operator.SUBTRACT,  Pitch p2]:

                    PitchDiff((Pitch)p1, (Pitch)p2, tokens);
                    break;

                case [Pitch p, 
                     Operator op and (Operator.ADD or Operator.SUBTRACT), Interval i]:

                    PitchInc((Operator)op, (Pitch)p, (Interval)i, tokens);
                    break;

                default:
                    throw new ArgumentException("Unknown operation or expression");
            }
        }

        if (tokens.Count() == 1)
            Console.WriteLine(tokens.Dequeue());
    }
}

