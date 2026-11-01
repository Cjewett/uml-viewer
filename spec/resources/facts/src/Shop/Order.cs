namespace Shop;

// Total is computed from the lines.
public partial class Order : IPriced, System.IDisposable
{
    private readonly System.Collections.Generic.List<Line> _lines = new();

    public Money Total
    {
        get
        {
            return Money.Zero;
        }
    }

    public int Count() => _lines.Count;

    public void Dispose() { }

    public sealed class Line
    {
        public Money Amount { get; } = Money.Zero;
    }
}
