using System;
using System.IO;

internal class Program
{
    private static void Main(string[] args)
    {
        string input = (args.Length > 0) ? File.ReadAllText(args[0]) : ReadStdIn();

        Registry wordRegistry = new(input);

        Console.WriteLine(wordRegistry);
    }
    
    private static string ReadStdIn() => new StreamReader(Console.OpenStandardInput()).ReadToEnd();


    class Registry
    {
        record WordCount(string word, int count)
        {
            public override string ToString() => word.PadRight(20) + $"{count}";
        };

        List<WordCount> register;

        public Registry(string input)
        {
            List<string> words = new(input.Split(null));
            register = words.GroupBy(x => x)
                            .Select(g => new WordCount(g.Key, g.Count()))
                            .ToList();
            // TODO exclude empty items
        }

        public override string ToString() => String.Join("\n", register.Select(x => x.ToString()));
    }
}
