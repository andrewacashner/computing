using System;
using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
        ExitIfInputInvalid(args);
        string expr = args[0];

        string? note1, note2;
        note1 = note2 = null;

        try
        {
            Expression.Parse(expr, out note1, out note2);
        }
        catch(ArgumentException ex)
        {
            Console.Error.WriteLine(ex.Message);
            Environment.Exit(1);
        }
        
        if (note1 != null && note2 != null)
        {
            Pitch? pitch1, pitch2;
            pitch1 = pitch2 = null;

            try
            {
                pitch1 = new Pitch(note1);
                pitch2 = new Pitch(note2);
            }
            catch(ArgumentException ex)
            {
                string error = $"Could not create Pitch: {ex.Message}";
                Console.Error.WriteLine(error);
                Environment.Exit(1);
            }

            if (pitch1 != null && pitch2 != null)
            {
                try 
                {
                    Interval interval = new(pitch1, pitch2);
                    Console.WriteLine($"{pitch2} - {pitch1} = {interval}");
                }
                catch(ArgumentException ex)
                {
                    Console.Error.WriteLine($"Problem creating interval: {ex.Message}");
                    Environment.Exit(1);
                }
            }
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


