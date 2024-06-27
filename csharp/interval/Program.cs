using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
        ExitIfInputInvalid(args);
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
              PITCH [+-] INTERVAL    (=> pitch)
            Or a combination, e.g.:
              PITCH1 + INTERVAL - INTERVAL => pitch

            Pitch is written as 'Pitch(c#)'
            Interval is written as 'Interval(m2)'

            Examples: 
                'Pitch(Ab) - Interval(m2) + Interval(P5)' (Result: 'D♮')
                'Pitch(d) - Interval(m2) - Pitch(b)'      (Result: 'M2')
        """;

        if (args.Length != 1)
        {
            Console.Error.WriteLine(USAGE);
            Environment.Exit(1);
        }
    }
}


