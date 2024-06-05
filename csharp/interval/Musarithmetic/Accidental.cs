namespace Musarithmetic;

public class Accidental
{
    enum kAccidental { DOUBLE_FLAT, FLAT, NATURAL, SHARP, DOUBLE_SHARP };

    kAccidental accidental;
   
    int Value { get => (int)accidental; }
    
    public int Adjustment { get => this.Value - 2; }

    public Accidental(int adjustment)
    {
        accidental = (kAccidental)(adjustment + 2);

        if (!Enum.IsDefined<kAccidental>(accidental))
            throw new ArgumentException($"Adjustment value {adjustment} is not valid for creating an accidental");
    }

    public Accidental(string accidStr)
    {
        accidental = accidStr switch 
        {
            "bb" => kAccidental.DOUBLE_FLAT,
            "b"  => kAccidental.FLAT,
            ""   => kAccidental.NATURAL,
            "#"  => kAccidental.SHARP,
            "##" => kAccidental.DOUBLE_SHARP,
            _    => throw new ArgumentException($"""
                        Unrecognized accidental input '{accidStr}'
                        (Acceptable inputs: 'bb', 'b', '#', '##', or nothing if natural)
                        """)
        };
    }

    public override string ToString() =>
        (new[] {"𝄫", "♭", "♮", "♯", "𝄪"}).ElementAt(this.Value);

}


