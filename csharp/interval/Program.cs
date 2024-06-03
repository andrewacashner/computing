using System;
using System.Collections.Generic;
using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
    //    ExitIfInputInvalid(args);
        string expr = args[0];


        Queue<object> tokens; 
        try
        {
            tokens = Expression.Parse(expr);
            Expression.Evaluate(tokens);
        }
        catch(ArgumentException ex)
        {
            Console.Error.WriteLine(ex.Message);
            Environment.Exit(1);
        }
    }

    static void ExitIfInputInvalid(string[] args)
    {
        const string USAGE = """
        Usage: interval 'EXPRESSION'
            EXPRESSION may be one of the following:
              PITCH1 - PITCH2       (=> interval)
              PITCH [+-] INTERVAL    (=> pitch2)
              INTERVAL [*/] N          (=> interval)
            Or a combination, e.g.:
              PITCH1 + (PITCH2 - PITCH3) - (INTERVAL * N) => pitch4
        """;

        if (!(args.Length == 1))
        {
            Console.Error.WriteLine(USAGE);
            Environment.Exit(1);
        }
    }
}


