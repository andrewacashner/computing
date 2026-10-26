namespace Pets;

public class Corgy 
(string name, int age)
: Dog(name, "Corgy", age)
{
    private int _Fatigue = 0;

    public bool IsTired() => (_Fatigue > 0);

    public override void Pet()
    {
        base.Pet();
        _Fatigue -= 5;
    }

    public override void Cuddle()
    {
        base.Cuddle();
        _Fatigue += 10;
    }

    public string TiredStatus()
    {
        string status = IsTired() ? "Norbert is tired" : "Norbert is good to go";
        return $"{status} (tiredness {_Fatigue})";
    }

}


