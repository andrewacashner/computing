namespace Musarithmetic;

public class Quality
{
    enum Qual { DIMINISHED, MINOR, MAJOR, PERFECT, AUGMENTED };
    bool IsPerfect(int interval) => interval is 0 or 3 or 4;

    Qual quality;

    public Quality(int degree, int adjustment)
    {
        quality = adjustment switch 
        {
            -2 => Qual.DIMINISHED, 
                -1 when IsPerfect(degree) => Qual.DIMINISHED,
                -1 => Qual.MINOR,
                0 when IsPerfect(degree) => Qual.PERFECT,
                0 => Qual.MAJOR,
                1 => Qual.AUGMENTED,
                _ => throw new ArgumentException($"Interval out of range")
        };
    }
    /*
       public int Value()
       {
       int qualityValue = qual switch
       {
       Qual.DIMINISHED => perfectIntervals.Contains(interval) ? -1 : -2,
       Qual.MINOR      => -1,
       Qual.MAJOR      => 0,
       Qual.PERFECT    => 0,
       Qual.AUGMENTED  => 1,
       _          => throw new ArgumentException($"Quality Value must be a Quality enum instance")
       };
       return qualityValue;
       }
       */
    public override string ToString()
    {
        string[] outputs = ["d", "m", "M", "P", "A"];
        return outputs[(int)quality];
    }


}


