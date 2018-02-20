coin_values = (100, 50, 25, 10, 5, 1)


def make_change(pennies):
    if pennies <= 0:
        return []

    # Find the largest coin whose value doesn't exceed pennies
    coin = _largest_not_exceeding(coin_values, pennies)
    return [coin] + make_change(pennies - coin)


def _largest_not_exceeding(descending_order_numbers, target):
    for n in descending_order_numbers:
        if n <= target:
            return n

    assert not "Whoops"


def test_em_all():
    assert make_change(1) == [1]
    assert make_change(2) == [1, 1]
    assert make_change(3) == [1, 1, 1]
    assert make_change(4) == [1, 1, 1, 1]
    assert make_change(5) == [5]
    assert make_change(10) == [10]
    assert make_change(11) == [10, 1]
    assert make_change(101) == [100, 1]
    assert make_change(110) == [100, 10]
    assert make_change(111) == [100, 10, 1]
    assert make_change(112) == [100, 10, 1, 1]
    assert make_change(212) == [100, 100, 10, 1, 1]
