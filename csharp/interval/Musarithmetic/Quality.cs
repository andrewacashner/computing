namespace Musarithmetic;

public class Quality
{
    enum kQuality{ DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };

    kQuality quality;

    int value { get => (int)quality; }

    public Quality(int degree, int adjustment)
    {
        quality = adjustment switch 
        {
            -2 => kQuality.DIMINISHED, 
            -1 when IsPerfectDegree(degree) => kQuality.DIMINISHED,
            -1 => kQuality.MINOR,
            0 when IsPerfectDegree(degree) => kQuality.PERFECT,
            0 => kQuality.MAJOR,
            1 => kQuality.AUGMENTED,
            _ => throw new ArgumentException($"Interval out of range")
        };
    }

    public Quality(string qualityStr)
    {
        quality = qualityStr switch
        {
            "d" => kQuality.DIMINISHED,
            "m" => kQuality.MINOR,
            "M" => kQuality.MAJOR,
            "P" => kQuality.PERFECT,
            "a" => kQuality.AUGMENTED,
            _ => throw new ArgumentException(
                    $"Unrecognized quality input {qualityStr}")
        };
    }

    public override string ToString() => 
        (new [] {"d", "m", "M", "P", "A"}).ElementAt(this.value);
    
    public int Adjustment(int degree) 
    {
        return quality switch
        {
            kQuality.DIMINISHED => IsPerfectDegree(degree) ? -1 : -2,
            kQuality.MINOR => -1,
            kQuality.MAJOR or kQuality.PERFECT => 0,
            kQuality.AUGMENTED => 1,
            _ => throw new ArgumentException(
            $"Cannot calculate chromatic value of interval quality {quality}")
        };
    }

    static bool IsPerfectDegree(int interval) => (interval % 7) is 0 or 3 or 4;

    bool IsPerfect() =>
        quality is kQuality.DIMINISHED or kQuality.PERFECT or kQuality.AUGMENTED;

    bool IsImperfect() =>
        quality is kQuality.DIMINISHED or kQuality.MINOR 
            or kQuality.MAJOR or kQuality.AUGMENTED;

    public bool MatchesDegree(int degree)
    {
        int reduced = Math.Abs(degree % 7);
        return (IsPerfect() && IsPerfectDegree(reduced))
            || (IsImperfect() && !IsPerfectDegree(reduced));
    }

}


