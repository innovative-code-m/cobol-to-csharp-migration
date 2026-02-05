namespace CobolToCsharpMigration;

/// <summary>
/// 注文サービス
/// </summary>
public class OrderService
{
    /// <summary>
    /// 注文を検証する
    /// </summary>
    /// <param name="amount">注文金額</param>
    /// <returns>金額が正の値の場合は true、0以下の場合は false</returns>
    public bool ValidateOrder(decimal amount)
    {
        if (amount <= 0)
        {
            return false;
        }

        return true;
    }
}
