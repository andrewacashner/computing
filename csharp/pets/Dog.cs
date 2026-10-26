namespace Pets;

public class Dog 
(string name, string breed, int age)
: AnimalPet(name, "dog", breed , age, Rewards)
{
    private static readonly HappinessRewards Rewards = new(100, 100, 200, -20);
}
