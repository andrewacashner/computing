namespace Musarithmetic;

public enum PitchName { PC_C, PC_D, PC_E, PC_F, PC_G, PC_A, PC_B };

public static class PitchNameHelper
{

    public static PitchName FromString(string pnameStr)
    {
        return pnameStr.ToUpper() switch
        {
            "C" => PitchName.PC_C,
            "D" => PitchName.PC_D,
            "E" => PitchName.PC_E,
            "F" => PitchName.PC_F,
            "G" => PitchName.PC_G,
            "A" => PitchName.PC_A,
            "B" => PitchName.PC_B,
            _   => throw new ArgumentException(
                    $"Unrecognized pitch name '{pnameStr}' (Must be a letter A-G)")
        };
    }

    public static string ToSymbol(this PitchName pname)
    {
        string[] outputs = ["C", "D", "E", "F", "G", "A", "B"];
        return outputs[(int)pname];
    }

    public static int DiatonicOffset(this PitchName pname) => (int)pname;

    public static int ChromaticOffset(int degree)
    {
        int[] offsets = [0, 2, 4, 5, 7, 9, 11];
        return offsets[degree];
    }

    public static int ChromaticOffset(this PitchName pname) =>
        ChromaticOffset((int)pname);
}
