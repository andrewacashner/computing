using Binary;

internal class Program
{
    private static void Main(string[] args)
    {
        if (!(args.Length == 1))
        {
            Console.Error.WriteLine("Usage: binary INTEGER");
            Environment.Exit(1);
        }

        try 
        {
            var given = int.Parse(args[0]);
            Console.WriteLine(given.ToBinary());
        }
        catch (Exception e)
        {
            Console.Error.WriteLine(e.Message);
            Environment.Exit(1);
        }
    }
}

namespace Binary
{
    static class BinaryString
    {
        public static string ToBinary(this int n)
        {
            int quot = n;
            var output = new List<char>();
            do
            {
                (quot, int rem) = Math.DivRem(quot, 2);
                output.Add(rem == 1 ? '1' : '0');
            } while (quot > 0);

            output.Reverse(); 
            return new string(output.ToArray());
        }
    }
}
