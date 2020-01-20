import functools
import itertools


def memoize(func):
    cache = {}

    @functools.wraps(func)
    def _inner(arg):
        if arg in cache:
            return cache[arg]

        result = func(arg)
        cache[arg] = result
        return result
    return _inner


@memoize
def fact(n):
    n = abs(int(n))
    if n <= 1:
        return 1
    return n * fact(n - 1)


sum = 0
for n in itertools.count(1):
    sum += 1 / fact(n)
    print(sum)
    if n > 20:
        break
