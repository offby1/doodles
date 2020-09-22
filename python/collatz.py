# https://en.wikipedia.org/wiki/3x_%2B_1_problem
import functools
import itertools


@functools.lru_cache(maxsize=None)
def hotpo(x):
    q, r = divmod(x, 2)
    if r == 0:
        return q
    else:
        return 1 + 3 * x


def cycle_length(starting_value):
    seen_values = set()
    for index in itertools.count():
        if starting_value in seen_values:
            return index
        seen_values.add(starting_value)
        starting_value = hotpo(starting_value)


def find_big_cyles():
    successively_longer_cycles = []
    for starting_value in itertools.count():
        toople = starting_value, cycle_length(starting_value)

        if not successively_longer_cycles:
            successively_longer_cycles = [toople]

        if toople[1] > successively_longer_cycles[-1][1]:
            successively_longer_cycles.append(toople)
            print(successively_longer_cycles[-1])

        if len(successively_longer_cycles) == 35:
            break


if __name__ == "__main__":
    find_big_cyles()
    print(hotpo.cache_info())
