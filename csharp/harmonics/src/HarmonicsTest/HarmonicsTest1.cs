namespace HarmonicsTest;

public class UnitTest1
{
    const int NUM_PARTIALS = 6;

    [Fact]
    public void ThrowsExceptionForNegativeFundamental()
    {
        const int NEGATIVE_FUNDAMENTAL = -1;
        Assert.Throws<ArgumentException>(() => HarmonicSeries.Generate(NEGATIVE_FUNDAMENTAL, NUM_PARTIALS));
    }

    [Theory]
    [InlineData(110)]
    [InlineData(415)]
    public void HarmonicSeriesIsCorrect(int fundamental)
    {
        int[] partials = HarmonicSeries.Generate(fundamental, NUM_PARTIALS);
        bool partialsValid = 
            partials.Select((partial, index) => partial == fundamental * (index + 1))
                    .All(result => result);
        Assert.True(partialsValid);
    }
}
