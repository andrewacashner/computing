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

/*
 * public static bool IsValidInput(string input)
    {
        bool IsValidPnameInput(char input) => 
            ["ABCDEFG"].Contains(Char.ToUpper(input));

        bool IsValidAccidInput(string input) => 
            ["bb", "b", "#", "##"].Contains(input);
        
        return input != "" && IsValidPnameInput(input[0])
            && (input.Length == 1 || IsValidAccidInput(input[1..]));
    }
    */

    public int DiatonicValue() => pname.DiatonicOffset();

    public int ChromaticValue()
    {
        int offset = pname.ChromaticOffset();
        int adjustment = accid.Adjustment();
        return offset + adjustment;
    }

}
