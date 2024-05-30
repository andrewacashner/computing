namespace Musarithmetic;

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


