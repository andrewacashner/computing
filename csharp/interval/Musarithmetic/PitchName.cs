namespace Musarithmetic;

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
