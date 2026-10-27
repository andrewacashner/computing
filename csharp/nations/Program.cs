namespace Nation;

public class Nation
{
    public static void Main(string[] args)
    {
        Cosmopolitan Joseph = new("Tom");
        USAmerican Joe = new("Joe");
        USAmerican Kamala = new("Kamala");
        Mexican Jose = new("José");
        Mexican Maria = new("María");

        List<Person> people = [Joseph, Joe, Jose];
        List<USAmerican> americans = [Joe, Kamala];
        List<Mexican> mexicans = [Jose, Maria];

        Console.WriteLine("Affiliations of people as their own class:");
        Console.WriteLine(Joseph.Affiliation());
        Console.WriteLine(Joe.Affiliation());
        Console.WriteLine(Jose.Affiliation());

        Console.WriteLine("\nAffiliations of people using generic method:");
        Console.WriteLine(AffiliationSummary(people));

        Console.WriteLine("\nAffiliations of people using parent class:");
        Console.WriteLine(AffiliationSummary2(people));

        Console.WriteLine("\nAffiliations of Americans using generic method:");
        Console.WriteLine(AffiliationSummary(americans));

        Console.WriteLine("\nAffiliations of Mexicans using generic method:");
        Console.WriteLine(AffiliationSummary(mexicans));
        
        /* NB AffiliationSummary2(americans) will not compile */

        Console.WriteLine("\nAffiliations of Americans using parent class:");
        List<Person> AmericansAsPeople = 
            [.. americans.Select(american => (Person) american)];
        Console.WriteLine(AffiliationSummary2(AmericansAsPeople));

        Console.WriteLine("\nAffiliations of Mexicans using parent class:");
        List<Person> MexicansAsPeople = 
            [.. mexicans.Select(mexican => (Person) mexican)];
        Console.WriteLine(AffiliationSummary2(MexicansAsPeople));
    }


    private static string AffiliationSummary<T>(List<T> people) where T : Person
    {
        return string.Join("\n", people.Select(person => person.Affiliation()));
    }
    
    private static string AffiliationSummary2(List<Person> people) 
    {
        return string.Join("\n", people.Select(person => person.Affiliation()));
    }
}
