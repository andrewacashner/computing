namespace Musarithmetic;

public class Quality
{
    public enum Qual { DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };
    public static bool IsPerfectDegree(int interval) => interval is 0 or 3 or 4;

    public Qual quality;
    
    public static string[] ValidInputs = ["d", "m", "M", "P", "A"];
    public static string[] ValidOutputs = ValidInputs;

    public Quality(Qual q)
    {
        quality = q;
    }

    public Quality(int degree, int adjustment)
    {
        quality = adjustment switch 
        {
            -2 => Qual.DIMINISHED, 
                -1 when IsPerfectDegree(degree) => Qual.DIMINISHED,
                -1 => Qual.MINOR,
                0 when IsPerfectDegree(degree) => Qual.PERFECT,
                0 => Qual.MAJOR,
                1 => Qual.AUGMENTED,
                _ => throw new ArgumentException($"Interval out of range")
        };
    }

    public Quality(char input)
    {
        quality = input switch
        {
            'd' => Qual.DIMINISHED,
            'm' => Qual.MINOR,
            'M' => Qual.MAJOR,
            'P' => Qual.PERFECT,
            'a' => Qual.AUGMENTED,
            _ => throw new ArgumentException($"Unrecognized quality input {input}")
        };

    }
   

    public override string ToString()
    {
        return ValidOutputs[(int)quality];
    }
    
    public bool IsPerfect()
    {
        Qual[] perfectQualities = [
            Qual.DIMINISHED, 
            Qual.PERFECT,
            Qual.AUGMENTED
        ];
        return perfectQualities.Contains(this.quality);
    }

    public bool IsImperfect()
    {
        Qual[] imperfectQualities = [
            Qual.DIMINISHED, 
            Qual.MINOR, 
            Qual.MAJOR, 
            Qual.AUGMENTED
        ];
        return imperfectQualities.Contains(this.quality);
    }

}


