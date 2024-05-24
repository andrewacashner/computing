using System;
using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
        ExitIfInputInvalid(args);

        string note1, note2;
        ReadInput(args, out note1, out note2);

        Pitch pitch1 = new(note1);
        Pitch pitch2 = new(note2);

        // Calculate interval
        Console.WriteLine($"Input: {note1} to {note2}");
        Console.WriteLine($"Output: {pitch1} to {pitch2}");
    }

    static void ExitIfInputInvalid(string[] args)
    {
        const string USAGE = """
        Usage: interval NOTE1 NOTE2
            (NOTE: A, Ab, Abb, A#, or A##)
        """;

        if (args.Length != 2)
        {
            Console.Error.WriteLine(USAGE);
            Environment.Exit(1);
        }
    }

    static void ReadInput(string[] args, out string note1, out string note2)
    {
        note1 = args[0];
        note2 = args[1];
    }
}

namespace Musarithmetic
{
    public class Pitch
    {
        char pname;
        string accid; // replace with enum or class?

        public Pitch(char pname, string accid)
        {
            this.pname = pname;
            this.accid = accid;
        }

        public Pitch(string rawInput)
        {
            this.pname = Char.ToUpper(rawInput[0]);
            this.accid = rawInput[1..];
        }

        public override string ToString()
        {
            return $"{this.pname}{this.accid}";
        }
    }
}
