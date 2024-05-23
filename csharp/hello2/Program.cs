using System;
using Salutations;

internal class Program
{
    private static void Main(string[] args)
    {
        const string USAGE = "Usage: hello NAME REPETITIONS";

        if (args.Length != 2) {
            Console.WriteLine(USAGE);
            Environment.Exit(1);
        }
        string name = args[0];
        int repetitions = 0;
        bool test = Int32.TryParse(args[1], out repetitions);
        if (!test) {
            Console.WriteLine("Bad input: REPETITIONS must be an integer");
            Console.WriteLine(USAGE);
            Environment.Exit(1);
        }
        string names = Greeting.Replicate(name, repetitions);
        string msg = Greeting.Hello(names);
        Console.WriteLine(msg);
    }
}

namespace Salutations
{
    public class Greeting
    {
        public static string Replicate(string msg, int repetitions)
        {
            string baseMsg = "";
            for (int i = 0; i < repetitions; ++i)
                baseMsg += msg;

            return baseMsg;
        }

        public static string Hello(string name)
        {
            return $"Hello, {name}!";
        }
    }
}

