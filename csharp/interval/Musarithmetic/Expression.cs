namespace Musarithmetic;

using System.Collections.Generic;

public class Expression
{
    public static List<object> Parse(string expr)
    {
        void ThrowNoParseError(string input) 
            => throw new ArgumentException($"Could not parse input '{input}'");
       
        bool IsValidOperandInput(string inputStr)
        {
            return (inputStr.StartsWith("Interval(") 
                    || inputStr.StartsWith("Pitch("))
                && inputStr.EndsWith(")");
        }

        void AppendNewOperator(string operatorStr, List<object> tokens)
        {
            try
            {
                Operator op = OperatorHelper.FromString(operatorStr);
                tokens.Add(op);
            }
            catch { throw; }
        }

        void AppendNewPitch(string pitchStr, List<object> tokens)
        {
            try
            {
                Pitch p = new(pitchStr);
                tokens.Add(p);
            }
            catch { throw; }
        }

        void AppendNewInterval(string intervalStr, List<object> tokens)
        {
            Interval i;
            try 
            {
                i = Interval.FromString(intervalStr);
                tokens.Add(i);
            }
            catch { throw; }
        }

        List<object> tokens = new();

        string[] words = expr.Split(' ');
        
        foreach (string thisWord in words)
        {
            switch (thisWord)
            {
                case "-":
                case "+":
                    AppendNewOperator(thisWord, tokens);
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
                        AppendNewPitch(arg, tokens);
                        break;

                    case ["Interval", string arg]:
                        AppendNewInterval(arg, tokens);
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

    public static void Evaluate(List<object> tokens)
    {
      
        // TODO is there a better way?
        // Need to access input tokens in FIFO order
        // But need to process them in groups of three and then push the
        // results back to the front of the list (which is LIFO)
        // So currently, we save inputs in a List, then reverse the list and
        // convert it to a Stack, then pop from and push to the Stack for
        // evaluating.  
        List<object> lifoTokens = new(tokens);
        lifoTokens.Reverse();
        
        Stack<object> tokenStack = new(lifoTokens);

        object[] stream = new object[3];

        while (tokenStack.Count() >= 3)
        {
            for (int i = 0; i < 3; ++i)
            {
                stream[i] = tokenStack.Pop();
                Console.Write(stream[i].ToString() + " ");
            }
            Console.WriteLine();

            switch (stream)
            {
                case [Pitch p1, Operator.SUBTRACT,  Pitch p2]:

                    tokenStack.Push(new Interval((Pitch)p1, (Pitch)p2));
                    break;

                case [Pitch p, 
                     Operator op and (Operator.ADD or Operator.SUBTRACT), 
                     Interval i]:

                    throw new ArgumentException("I don't know how to add or subtract intervals from pitches yet");

                default:
                    throw new ArgumentException("Unknown operation or expression");
            }
        }

        if (tokenStack.Count() == 1)
            Console.WriteLine(tokenStack.Pop());
    }
}

