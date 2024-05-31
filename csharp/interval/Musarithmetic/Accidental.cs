namespace Musarithmetic;

public enum Accidental { DOUBLE_FLAT, FLAT, NATURAL, SHARP, DOUBLE_SHARP };

public static class AccidentalHelper
{
    public static Accidental FromString(string accidStr)
    {
        return accidStr switch 
        {
            "bb" => Accidental.DOUBLE_FLAT,
            "b"  => Accidental.FLAT,
            ""   => Accidental.NATURAL,
            "#"  => Accidental.SHARP,
            "##" => Accidental.DOUBLE_SHARP,
            _    => throw new ArgumentException($"""
                        Unrecognized accidental input '{accidStr}'
                        (Acceptable inputs: 'bb', 'b', '#', '##', or nothing if natural)
                        """)
        };
    }

    public static string ToSymbol(this Accidental accid) 
    {
        string[] outputs = ["𝄫", "♭", "♮", "♯", "𝄪"];
        return outputs[(int)accid];
    }

    public static int Adjustment(this Accidental accid) => (int)accid - 2;
}


