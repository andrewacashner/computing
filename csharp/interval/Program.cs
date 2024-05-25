using System;
using Musarithmetic;

internal class Program
{
    private static void Main(string[] args)
    {
        ExitIfInputInvalid(args);

        string note1, note2;
        ReadInput(args, out note1, out note2);

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
                Console.WriteLine($"{pitch1} to {pitch2} = {interval}");
            }
            catch(ArgumentException ex)
            {
                Console.Error.WriteLine($"Problem creating interval: {ex.Message}");
                Environment.Exit(1);
            }
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
        enum Accid { DOUBLE_FLAT, FLAT, NATURAL, SHARP, DOUBLE_SHARP };

        Accid accid = Accid.NATURAL;

        public Accidental(string accidStr)
        {
            accid = accidStr switch 
            {
                "bb" => Accid.DOUBLE_FLAT,
                "b"  => Accid.FLAT,
                ""   => Accid.NATURAL,
                "#"  => Accid.SHARP,
                "##" => Accid.DOUBLE_SHARP,
                _    => throw new ArgumentException($"""
                        Unrecognized accidental input '{accidStr}'
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

    public class PitchName
    {
        enum Pname { PC_C, PC_D, PC_E, PC_F, PC_G, PC_A, PC_B };

        Pname pname;

        public PitchName(int offset)
        {
            pname = (Pname)offset;
        }

        public PitchName(char pnameChar)
        {
            pnameChar = Char.ToUpper(pnameChar);

            pname = pnameChar switch
            {
                'C' => Pname.PC_C,
                'D' => Pname.PC_D,
                'E' => Pname.PC_E,
                'F' => Pname.PC_F,
                'G' => Pname.PC_G,
                'A' => Pname.PC_A,
                'B' => Pname.PC_B,
                _   => throw new ArgumentException($"""
                        Unrecognized pitch name '{pnameChar}'
                            (Must be a letter A-G)
                        """)
            };
        }

        public override string ToString()
        {
            string[] outputs = ["C", "D", "E", "F", "G", "A", "B"];
            return outputs[(int)pname];
        }

        public int DiatonicOffset() => (int)pname;

        public int ChromaticOffset()
        {
            int[] offsets = [0, 2, 4, 5, 7, 9, 11];
            return offsets[(int)pname];
        }

        public static int ChromaticOffset(int degree)
        {
            return new PitchName(degree).ChromaticOffset();
        }
    }


    public class Pitch
    {
        PitchName pname;
        Accidental accid;

        public Pitch(string pitchStr)
        {
            try
            {
                pname = new PitchName(pitchStr[0]);
                accid = new Accidental(pitchStr[1..]);
            }
            catch { throw; }
        }

        public override string ToString()
        {
            return $"{this.pname}{this.accid}";
        }

        public int DiatonicValue() => pname.DiatonicOffset();
        
        public int ChromaticValue()
        {
            int offset = pname.ChromaticOffset();
            int adjustment = accid.Adjustment();
            return offset + adjustment;
        }

    }

    public class Quality
    {
        enum Qual { DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };
        bool IsPerfect(int interval) => interval is 0 or 3 or 4;

        Qual quality;
       
        public Quality(int degree, int adjustment)
        {
            quality = adjustment switch 
            {
                -2 => Qual.DIMINISHED, 
                -1 when IsPerfect(degree) => Qual.DIMINISHED,
                -1 => Qual.MINOR,
                 0 when IsPerfect(degree) => Qual.PERFECT,
                 0 => Qual.MAJOR,
                 1 => Qual.AUGMENTED,
                 _ => throw new ArgumentException($"Interval out of range")
            };
        }
/*
        public int Value()
        {
            int qualityValue = qual switch
            {
                Qual.DIMINISHED => perfectIntervals.Contains(interval) ? -1 : -2,
                Qual.MINOR      => -1,
                Qual.MAJOR      => 0,
                Qual.PERFECT    => 0,
                Qual.AUGMENTED  => 1,
                _          => throw new ArgumentException($"Quality Value must be a Quality enum instance")
            };
            return qualityValue;
        }
*/
        public override string ToString()
        {
            string[] outputs = ["d", "m", "M", "P", "A"];
            return outputs[(int)quality];
        }


    }

    public class Interval
    {
        Quality quality;
        int degree;

        public Interval(Pitch p1, Pitch p2)
        {
            degree = DiatonicInterval(p1, p2);
            
            int defaultInterval = PitchName.ChromaticOffset(degree);
            int inflectedInterval = ChromaticInterval(p1, p2);
            int adjustment = inflectedInterval - defaultInterval;

            try 
            {
                quality = new Quality(degree, adjustment);
            }
            catch { throw; }
        }


        static int loopDiff(int n, int m, int max)
        {
            if (m < n)
                m += max;

            return m - n;
        }

        // TODO replace this and next with generics?
        static int DiatonicInterval(Pitch p1, Pitch p2)
        {
            int val1 = p1.DiatonicValue();
            int val2 = p2.DiatonicValue();
            return loopDiff(val1, val2, 7);
        }
        
        static int ChromaticInterval(Pitch p1, Pitch p2)
        {
            int val1 = p1.ChromaticValue();
            int val2 = p2.ChromaticValue();
            return loopDiff(val1, val2, 12);
        }


        public override string ToString()
        {
            return $"{quality}{degree + 1}";
        }
    }

}
