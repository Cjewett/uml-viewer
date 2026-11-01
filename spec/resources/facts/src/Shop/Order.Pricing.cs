namespace Shop;

public partial class Order
{
    public decimal Price(Tier tier) => tier == Tier.Gold ? 0m : 1m;
    public decimal Price() => Price(Tier.Basic);
}
