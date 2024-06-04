namespace Musarithmetic;
using System.Diagnostics;

public class Accidental
{
    enum Accid { DOUBLE_FLAT, FLAT, NATURAL, SHARP, DOUBLE_SHARP };

    Accid accidental;
   
    public int Value { get => (int)accidental; }
    public int Adjustment { get => this.Value - 2; }

    public Accidental(int adjustment)
    {
        accidental = (Accid)(adjustment + 2);

        if (!Enum.IsDefined<Accid>(accidental))
            throw new ArgumentException($"Adjustment value {adjustment} is not valid for creating an accidental");
    }

    public Accidental(string accidStr)
    {
        accidental = accidStr switch 
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

    public override string ToString() =>
        (new[] {"𝄫", "♭", "♮", "♯", "𝄪"}).ElementAt(this.Value);

}


