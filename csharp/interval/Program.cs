using System;
using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
        ExitIfInputInvalid(args);

        string note1, note2;
        ReadInput(args, out note1, out note2);
        Console.WriteLine($"Input: {note1} to {note2}");

        Pitch? pitch1, pitch2;
        pitch1 = pitch2 = null;

        try
        {
            pitch1 = new Pitch(note1);
            pitch2 = new Pitch(note2);
            Console.WriteLine($"Output: {pitch1} to {pitch2}");
        }
        catch(ArgumentException ex)
        {
            string error = $"Could not create Pitch: {ex.Message}";
            Console.Error.WriteLine(error);
            Environment.Exit(1);
        }

        if (pitch1 != null && pitch2 != null)
        {
            Interval interval = new(pitch1, pitch2);
            Console.WriteLine($"{interval}");
        }
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
    
    
    public class Accidental
    {
        public enum Accid { DOUBLE_FLAT, FLAT, NATURAL, SHARP, DOUBLE_SHARP }

        Accid accid = Accid.NATURAL;

        public Accidental(Accid a) => accid = a;

        public Accidental(string input)
        {
            accid = input switch 
            {
                "bb" => Accid.DOUBLE_FLAT,
                "b"  => Accid.FLAT,
                ""   => Accid.NATURAL,
                "#"  => Accid.SHARP,
                "##" => Accid.DOUBLE_SHARP,
                _    => throw new ArgumentException($"""
                        Unrecognized accidental input '{input}'
                            (Acceptable inputs: 'bb', 'b', '#', '##', or nothing if natural)
                        """)
            };

        }

        public override string ToString() 
        {
            string[] outputs = ["𝄫", "♭", "♮", "♯", "𝄪"];
            return outputs[(int)accid];
        }

        public int Adjustment() => (int)accid - 2;
    }

    public class Pitch
    {
        char pname;
        Accidental accid;

        public Pitch(char pname, Accidental accid)
        {
            this.pname = pname;
            this.accid = accid;
        }

        public Pitch(string rawInput)
        {
            char letter = Char.ToUpper(rawInput[0]);
            if ("ABCDEFG".Contains(letter))
                pname = letter;
            else
                throw new ArgumentException($"Bad Pitch input '{rawInput}'" +
                        " (First character must be a letter A-G)");

            try
            {
                string accidStr = rawInput[1..];
                accid = new Accidental(accidStr);
            }
            catch { throw; }
        }

        public override string ToString()
        {
            return $"{this.pname}{this.accid}";
        }

        public int DiatonicValue()
        {
            return "CDEFGAB".IndexOf(pname);
        }
    }

    public class Interval
    {
        // quality
        int degree;

        public Interval(Pitch p1, Pitch p2)
        {
            degree = p2.DiatonicValue() - p1.DiatonicValue();
        }

        public override string ToString()
        {
            return $"{degree + 1}";
        }
    }

}
