namespace Nation;

public class Mexican
(string name)
: Person(name)
{
    public new string Affiliation()
    {
        return $"I am {_Name}, a Mexican citizen.";
    }
}
