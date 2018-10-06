# https://en.wikipedia.org/wiki/3x_%2B_1_problem
import itertools


def repeated_calls(function, argument):
    result = argument
    while True:
        yield result
        result = function(result)


def hotpo(x):
    q, r = divmod(x, 2)
    if r == 0:
        return q
    else:
        return 1 + 3 * x


print(list(itertools.islice(repeated_calls(hotpo, 27), 111)))
