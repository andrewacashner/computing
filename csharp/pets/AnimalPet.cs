namespace Pets;

public record HappinessRewards(int Baseline, int Pet, int Cuddle, int Ignore) {}

public abstract class AnimalPet
(string name, string species, string breed, int age, HappinessRewards rewards)
: Creature(rewards.Baseline), IPettable
{
    protected string _Name = name;
    protected string _Species = species;

    protected string _Breed = breed;
    protected int _Age = age;

    protected HappinessRewards _Rewards = rewards;
    
    private static readonly HappinessRewards DefaultRewards = new(50, 100, 200, -50);

    protected virtual void ApplyReward(int reward)
    {
        _Happiness += reward;
    }
    public AnimalPet(string name, string species, string breed, int age)
    : this(name, species, breed, age, DefaultRewards)
    {}

    protected void PrintActionMsg(string whatYouDid)
    {
        Console.WriteLine($"You {whatYouDid} {_Name}");
    }
    public virtual void Pet() 
    {
        PrintActionMsg("pet");
        ApplyReward(_Rewards.Pet);
    }

    public virtual void Cuddle() 
    {
        PrintActionMsg("cuddled");
        ApplyReward(_Rewards.Cuddle);
    }

    public virtual void Ignore() 
    {
        PrintActionMsg("ignored");
        ApplyReward(_Rewards.Ignore);
    }
    public override string ToString()
    {
        return $"{_Name} ({_Breed} {_Species}, age {_Age})";
    }
}