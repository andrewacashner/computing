namespace Musarithmetic;

public class PitchName
{
    enum kPitchName { PC_C, PC_D, PC_E, PC_F, PC_G, PC_A, PC_B };

    kPitchName pitchName;

    int Value { get => (int)pitchName; }

    public PitchName(int degree)
    {
        pitchName = (kPitchName)(Math.Abs(degree % 7));

        if (!Enum.IsDefined<kPitchName>(pitchName))
            throw new ArgumentException($"Degree value {degree} is not valid for creating a PitchName");
    }

    public PitchName(string pnameStr)
    {
        pitchName = pnameStr.ToUpper() switch
        {
            "C" => kPitchName.PC_C,
            "D" => kPitchName.PC_D,
            "E" => kPitchName.PC_E,
            "F" => kPitchName.PC_F,
            "G" => kPitchName.PC_G,
            "A" => kPitchName.PC_A,
            "B" => kPitchName.PC_B,
            _   => throw new ArgumentException(
                    $"Unrecognized pitch name '{pnameStr}' (Must be a letter A-G)")
        };
    }

    public override string ToString()
        => (new [] {"C", "D", "E", "F", "G", "A", "B"}).ElementAt(this.Value);

    public int DiatonicValue { get => this.Value; }

    public int ChromaticValue { get => 
        (new [] {0, 2, 4, 5, 7, 9, 11}).ElementAt(this.Value); }

}
