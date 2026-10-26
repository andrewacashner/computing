namespace Pets;

public class Cat 
(string name, string breed, int age, int grumpiness)
: AnimalPet(name, "cat", breed, age, Rewards)
{
    protected readonly int _Grumpiness = grumpiness;

    private static readonly HappinessRewards Rewards = new(-10, 5, 10, -10);
    private bool IsGrumpy() => (_Grumpiness > 50);

    private void TryToPlease(int reward)
    {
        int effect = IsGrumpy() ? -1 : 1;
        _Happiness += effect * reward;
    }

    protected override void ApplyReward(int reward) => TryToPlease(reward);
}
