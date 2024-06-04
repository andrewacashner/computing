namespace Musarithmetic;

public class Quality
{
    enum Qual{ DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };

    Qual quality;

    public int value { get => (int)quality; }

    public static bool IsPerfectDegree(int interval) => 
        (interval % 7) is 0 or 3 or 4;

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

    public Quality(string qualityStr)
    {
        quality = qualityStr switch
        {
            "d" => Qual.DIMINISHED,
            "m" => Qual.MINOR,
            "M" => Qual.MAJOR,
            "P" => Qual.PERFECT,
            "a" => Qual.AUGMENTED,
            _ => throw new ArgumentException($"Unrecognized quality input {qualityStr}")
        };
    }

    public override string ToString() => 
        (new [] {"d", "m", "M", "P", "A"}).ElementAt(this.value);
    
    public bool IsPerfect() =>
        quality is Qual.DIMINISHED or Qual.PERFECT or Qual.AUGMENTED;

    public bool IsImperfect() =>
        quality is Qual.DIMINISHED or Qual.MINOR 
            or Qual.MAJOR or Qual.AUGMENTED;

    public int Adjustment(int degree) 
    {
        return quality switch
        {
            Qual.DIMINISHED => IsPerfectDegree(degree) ? -1 : -2,
            Qual.MINOR => -1,
            Qual.MAJOR or Qual.PERFECT => 0,
            Qual.AUGMENTED => 1,
            _ => throw new ArgumentException($"Cannot calculate chromatic value of interval quality {quality}")
        };
    }

}


