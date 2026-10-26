using Pets;

class DogHouse 
{

    static void ReportStatus<T>(List<T> pets) where T: Creature
    {
        List<string> petStatus = [.. pets.Select(pet => pet.HappinessStatus())];
        Console.WriteLine($"\n{string.Join("\n", petStatus)}\n");
    }

    static void Main(string[] args)
    {
        Console.WriteLine("DOG HOUSE\n");

        Dog Fido = new("Fido", "German Shepherd", 2);
        Dog Rex = new("Rex", "Dachshund", 3);
        Corgy Norbert = new("Norbert", 1);

        List<Dog> dogs = [Fido, Rex, Norbert];
        ReportStatus(dogs);

        Fido.Pet();
        Rex.Ignore();
        Norbert.Pet();

        ReportStatus(dogs);

        Rex.Cuddle();
        Norbert.Ignore();

        ReportStatus(dogs);

        Norbert.Cuddle();

        Console.WriteLine(Norbert.TiredStatus());

        Norbert.Pet();
        Console.WriteLine(Norbert.TiredStatus());

        Cat Susan = new("Susan", "Domestic shorthair", 6, 10);
        Cat Lucy = new("Lucy", "Domestic shorthair", 6, 100);
        List<Cat> cats = [Susan, Lucy];

        ReportStatus(cats);

        Susan.Pet();
        Lucy.Pet();
        ReportStatus(cats);

        Susan.Cuddle();
        Lucy.Cuddle();
        ReportStatus(cats);

        Lucy.Ignore();
        ReportStatus(cats);
    }
}
