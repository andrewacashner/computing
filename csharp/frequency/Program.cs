using System;
using System.IO;

internal class Program
{
    private static void Main(string[] args)
    {
        string input = (args.Length > 0) 
                        ? File.ReadAllText(args[0]) 
                        : ReadStdIn();

        WordRegistry wordRegistry = new(input);
        Console.WriteLine(wordRegistry);
    }
    
    private static string ReadStdIn() => 
        new StreamReader(Console.OpenStandardInput()).ReadToEnd();


    class WordRegistry
    {
        List<WordCount> register;
        
        public record WordCount(string word, int count)
        {
            public override string ToString() => 
                word.PadRight(20) + $"{count}";
        }

        public WordRegistry(string input)
        {
            const string whitespace_punctuation = " \n\t,.:;!?\'\"\\/()[]{}<>";
            char[] separators = whitespace_punctuation.ToCharArray();

            List<string> words = new(input.Split(separators,
                                    StringSplitOptions.RemoveEmptyEntries));

            register = words.GroupBy(x => x)
                            .Select(g => new WordCount(g.Key, g.Count()))
                            .ToList();
        }

        public List<WordCount> SortedByFrequency() => 
            register.OrderByDescending(item => item.count)
                    .ThenBy(item => item.word, 
                            StringComparer.OrdinalIgnoreCase)
                    .ToList();

        public override string ToString() => 
            String.Join("\n", this.SortedByFrequency()
                                  .Select(item => item.ToString()));
    }
    
}
