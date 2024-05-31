namespace Musarithmetic;

public enum Quality { DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };

public static class QualityHelper
{
    public static bool IsPerfectDegree(int interval) => 
        interval is 0 or 3 or 4;

    public static Quality FromSize(int degree, int adjustment)
    {
        return adjustment switch 
        {
            -2 => Quality.DIMINISHED, 
            -1 when IsPerfectDegree(degree) => Quality.DIMINISHED,
            -1 => Quality.MINOR,
            0 when IsPerfectDegree(degree) => Quality.PERFECT,
            0 => Quality.MAJOR,
            1 => Quality.AUGMENTED,
            _ => throw new ArgumentException($"Interval out of range")
        };
    }

    public static Quality FromString(string qualityStr)
    {
        return qualityStr switch
        {
            "d" => Quality.DIMINISHED,
            "m" => Quality.MINOR,
            "M" => Quality.MAJOR,
            "P" => Quality.PERFECT,
            "a" => Quality.AUGMENTED,
            _ => throw new ArgumentException($"Unrecognized quality input {qualityStr}")
        };
    }

    public static string ToSymbol(this Quality quality)
    {
        string[] outputs = ["d", "m", "M", "P", "A"];
        return outputs[(int)quality];
    }
    
    public static bool IsPerfect(this Quality quality) =>
        quality is Quality.DIMINISHED or Quality.PERFECT or Quality.AUGMENTED;

    public static bool IsImperfect(this Quality quality) =>
        quality is Quality.DIMINISHED or Quality.MINOR 
        or Quality.MAJOR or Quality.AUGMENTED;
}


