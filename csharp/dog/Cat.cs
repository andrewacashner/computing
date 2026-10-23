namespace Pets;

public class Cat 
(string name, string breed, int age, int grumpiness)
    : IPettable
{
    private string Name => name;
    private string Breed => breed;
    private int Age => age;
    private int Grumpiness => grumpiness;
    private int Happiness = 0;


    private string ActionMsg(string whatYouDid)
    {
        return $"You {whatYouDid} {this.Name}";
    }

    private bool IsGrumpy()
    {
        return this.Grumpiness > 50;
    }

    private void TryToPlease(int amount)
    {
        int effect = IsGrumpy() ? -1 : 1;
        this.Happiness += effect * amount;
    }

    public virtual void Pet()
    {
        Console.WriteLine(ActionMsg("pet"));
        TryToPlease(50);
    }

    public virtual void Cuddle()
    {
        Console.WriteLine(ActionMsg("cuddled"));
        TryToPlease(100);
    }

    public virtual void Ignore()
    {
        Console.WriteLine(ActionMsg("ignored"));
        TryToPlease(-20); // a grumpy cat wants to be ignored
    }

    public override string ToString()
    {
        return $"{this.Name} ({this.Breed} cat, age {this.Age}) is at {this.Happiness} happiness";
    }
}
