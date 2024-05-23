internal class Program
{
    private static void Main(string[] args)
    {
        if (args.Length != 2) {
            Console.WriteLine("Usage: hello NAME");
            Environment.ExitCode = 1;
        } else {
            string name = args[1];
            Console.WriteLine(Greeting(name));
        }
    }
    static string Greeting(string name)
    {
        return $"Hello, {name}!";
    }
}