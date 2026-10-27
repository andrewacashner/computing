namespace HarmonicsTest;
using Harmonics;
public class UnitTest1
{
    const int DEFAULT_NUM_PARTIALS = 6;
    const int DEFAULT_FUNDAMENTAL = 440;

    [Fact]
    public void ThrowsExceptionForNegativeFundamental()
    {
        static HarmonicSeries series() => new(-1, DEFAULT_NUM_PARTIALS);
        Assert.Throws<ArgumentException>(series);
    }

    [Theory]
    [InlineData(110)]
    [InlineData(415)]
    public void HarmonicSeriesIsCorrect(int fundamental)
    {
        HarmonicSeries series = new(fundamental, DEFAULT_NUM_PARTIALS);
        bool partialsValid = 
            series.Partials.Select((partial, index) => partial == fundamental * (index + 1))
                    .All(result => result);
        Assert.True(partialsValid);
    }

    [Fact]
    public void ThrowsExceptionForNegativePartials()
    {
        static HarmonicSeries series() => new (DEFAULT_FUNDAMENTAL, -1);
        Assert.Throws<ArgumentException>(series);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(5)]
    [InlineData(25)]
    public void HarmonicSeriesHasCorrectNumberPartials(int numPartials)
    {
        HarmonicSeries partials = new(DEFAULT_FUNDAMENTAL, numPartials);
        Assert.True(partials.Length == numPartials);
    }

    [Theory]
    [InlineData("ratio")]
    [InlineData("4:b")]
    [InlineData("4:")]
    [InlineData(":4")]
    [InlineData("fish:squid")]
    public void AtRatioThrowsExceptionForInvalidInput(string input)
    {
        HarmonicSeries partials = new(DEFAULT_FUNDAMENTAL, DEFAULT_NUM_PARTIALS);
        Assert.Throws<ArgumentException>(() => partials.AtRatio(input));
    }

    [Fact]
    public void AtRatioThrowsExceptionForOutOfRange()
    {
        const string RATIO = "8:7";
        const int NUM_PARTIALS = 6;
        HarmonicSeries partials = new(DEFAULT_FUNDAMENTAL, NUM_PARTIALS);
        Assert.Throws<ArgumentException>(() => partials.AtRatio(RATIO));
    }

    [Theory]
    [InlineData(3, "4:3")]
    [InlineData(2, "3:2")]
    public void AtRatioMatchesCorrectPartial(int partial, string ratio)
    {
        HarmonicSeries series = new(DEFAULT_FUNDAMENTAL, DEFAULT_NUM_PARTIALS);
        int selectedPartial = series.AtRatio(ratio);
        Assert.True(series[partial] == selectedPartial);
    }
}
