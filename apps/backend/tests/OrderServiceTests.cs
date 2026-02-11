using Xunit;

namespace CobolToCsharpMigration.Tests;

/// <summary>
/// OrderService のテストクラス
/// </summary>
public class OrderServiceTests
{
    private readonly OrderService _orderService;

    public OrderServiceTests()
    {
        _orderService = new OrderService();
    }

    /// <summary>
    /// 金額が正の値の場合、true を返すことを検証
    /// </summary>
    [Fact]
    public void ValidateOrder_PositiveAmount_ReturnsTrue()
    {
        // Arrange
        decimal amount = 100.50m;

        // Act
        bool result = _orderService.ValidateOrder(amount);

        // Assert
        Assert.True(result);
    }

    /// <summary>
    /// 金額が0の場合、false を返すことを検証
    /// </summary>
    [Fact]
    public void ValidateOrder_ZeroAmount_ReturnsFalse()
    {
        // Arrange
        decimal amount = 0m;

        // Act
        bool result = _orderService.ValidateOrder(amount);

        // Assert
        Assert.False(result);
    }

    /// <summary>
    /// 金額が負の値の場合、false を返すことを検証
    /// </summary>
    [Fact]
    public void ValidateOrder_NegativeAmount_ReturnsFalse()
    {
        // Arrange
        decimal amount = -50.00m;

        // Act
        bool result = _orderService.ValidateOrder(amount);

        // Assert
        Assert.False(result);
    }

    /// <summary>
    /// 金額が非常に小さい正の値の場合、true を返すことを検証
    /// </summary>
    [Fact]
    public void ValidateOrder_VerySmallPositiveAmount_ReturnsTrue()
    {
        // Arrange
        decimal amount = 0.01m;

        // Act
        bool result = _orderService.ValidateOrder(amount);

        // Assert
        Assert.True(result);
    }

    /// <summary>
    /// 金額が大きい値の場合、true を返すことを検証
    /// </summary>
    [Fact]
    public void ValidateOrder_LargeAmount_ReturnsTrue()
    {
        // Arrange
        decimal amount = 1000000.00m;

        // Act
        bool result = _orderService.ValidateOrder(amount);

        // Assert
        Assert.True(result);
    }
}
