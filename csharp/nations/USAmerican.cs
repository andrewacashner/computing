namespace Nation;

public class USAmerican
(string name)
: Person(name)
{
    public override string Affiliation()
    {
        return $"I am {_Name}, a US citizen.";
    }
}

