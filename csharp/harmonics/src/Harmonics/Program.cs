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

            int[] partials = HarmonicSeries.Generate(fundamental, 6);

            string summary = string.Join("\n",
                [.. partials.Select(
                    (partial, index) => $"{index}: {partial} Hz")]);
            Console.WriteLine(summary);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Input must be an integer. {ex.Message}");
        }
    }
}
