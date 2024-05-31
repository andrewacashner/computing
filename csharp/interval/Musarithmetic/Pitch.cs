namespace Musarithmetic;

public class Pitch
{
    PitchName pname;
    Accidental accid;

    public Pitch(string pitchStr)
    {
        try
        {
            pname = PitchNameHelper.FromString(pitchStr[..1]);
            accid = AccidentalHelper.FromString(pitchStr[1..]);
        }
        catch { throw; }
    }

    public override string ToString() => 
        pname.ToSymbol() + accid.ToSymbol();

    public int DiatonicValue() => pname.DiatonicOffset();

    public int ChromaticValue()
    {
        int offset = pname.ChromaticOffset();
        int adjustment = accid.Adjustment();
        return offset + adjustment;
    }

}
