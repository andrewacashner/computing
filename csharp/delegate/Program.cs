using System;
using System.IO;

internal class Program
{
    private static void Main(string[] args)
    {
        string path = args.Length > 0 ? args.ElementAt(0) : "";
        string input = "";

        if (path == "")
        {
            string? thisLine;
            while ((thisLine = Console.ReadLine()) != null)
            {
                input += thisLine + "\n";
            }
        }
        else
        {
            input = File.ReadAllText(path);
        }

        Console.WriteLine(input);
    }
}
