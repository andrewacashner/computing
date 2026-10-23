namespace Pets;

public class Corgy : Dog, IPettable
{
    int Fatigue;

    public Corgy(string Name, int Age): base(Name, "Corgy", Age) 
    {
        this.Fatigue = 0;
    }

    public bool IsTired()
    {
        return (this.Fatigue > 0);
    }

    public override void Pet()
    {
        Console.WriteLine(ActionMsg("pet"));
        this.Happiness += 100;
        this.Fatigue -= 5;
    }

    public override void Cuddle()
    {
        Console.WriteLine(ActionMsg("cuddled"));
        this.Happiness += 200;
        this.Fatigue += 10;
    }

    public string TiredStatus()
    {
        string status = IsTired() ? "Norbert is tired" : "Norbert is good to go";

        return $"{status} (tiredness {this.Fatigue})";
    }

}


