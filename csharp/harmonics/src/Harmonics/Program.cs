namespace Harmonics;

public class HarmonicsApp
{
    public static void Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.Error.WriteLine("Usage: harmonics FUNDAMENTAL (Hz)");
            Environment.Exit(1);
        }

        string fundamentalStr = args[0];
        
        try
        {
            int fundamental = Int32.Parse(fundamentalStr);
            const int NUM_PARTIALS = 6;

            HarmonicSeries series = new(fundamental, NUM_PARTIALS);

            string summary = string.Join("\n",
                [.. series.Partials.Select(
                    (partial, index) => $"{index}: {partial} Hz")]);
            Console.WriteLine(summary);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Input must be an integer. {ex.Message}");
        }
    }
}
