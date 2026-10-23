namespace Pets;

public class Dog : IPettable 
{
    private string Name;
    private string Breed;
    private int Age;
    protected int Happiness;

    public Dog(string name, string breed, int age) 
    {
        this.Name = name;
        this.Breed = breed;
        this.Age = age;
        this.Happiness = 100;
    }

    protected string ActionMsg(string whatYouDid)
    {
        return $"You {whatYouDid} {this.Name}";
    }

    public virtual void Pet() 
    {
        Console.WriteLine(ActionMsg("pet"));
        this.Happiness += 100;
    }

    public virtual void Cuddle() 
    {
        Console.WriteLine(ActionMsg("cuddled"));
        this.Happiness += 200;
    }

    public virtual void Ignore() 
    {
        Console.WriteLine(ActionMsg("ignored"));
        this.Happiness -= 50;
    }

    public override string ToString() 
    {
        return $"{this.Name} ({this.Breed} dog, age {this.Age}) is at {this.Happiness} happiness";
    }
}
