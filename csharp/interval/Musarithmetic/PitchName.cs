namespace Musarithmetic;

public class PitchName
{
    enum Pname { PC_C, PC_D, PC_E, PC_F, PC_G, PC_A, PC_B };

    Pname pitchName;

    public int Value { get => (int)pitchName; }

    public PitchName(int degree)
    {
        pitchName = (Pname)(Math.Abs(degree % 7));

        if (!Enum.IsDefined<Pname>(pitchName))
            throw new ArgumentException($"Degree value {degree} is not valid for creating a PitchName");
    }

    public PitchName(string pnameStr)
    {
        pitchName = pnameStr.ToUpper() switch
        {
            "C" => Pname.PC_C,
            "D" => Pname.PC_D,
            "E" => Pname.PC_E,
            "F" => Pname.PC_F,
            "G" => Pname.PC_G,
            "A" => Pname.PC_A,
            "B" => Pname.PC_B,
            _   => throw new ArgumentException(
                    $"Unrecognized pitch name '{pnameStr}' (Must be a letter A-G)")
        };
    }

    public override string ToString()
        => (new [] {"C", "D", "E", "F", "G", "A", "B"}).ElementAt(this.Value);

    public int DiatonicOffset { get => this.Value; }
    public int ChromaticOffset { get => 
        (new [] {0, 2, 4, 5, 7, 9, 11}).ElementAt(this.Value); }

}
