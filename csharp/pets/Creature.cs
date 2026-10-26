namespace Pets;

public abstract class Creature
(int happiness)
{
    protected int _Happiness = happiness;

    public string HappinessStatus()
    {
        return $"{this} is at {_Happiness} happiness";
    }
}