namespace Nation;

public abstract class Person (string name)
{
    protected string _Name = name;

    public virtual string Affiliation()
    {
        return $"I am {_Name}, a citizen of the world.";
    }
}

