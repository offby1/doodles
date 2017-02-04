import itertools
import math

import progressbar              # pip install progressbar2


def columns(lol):
    for c in range(len(lol[0])):
        yield [row[c] for row in lol]


def se_diag(lol):
    for i, col in enumerate(columns(lol)):
        yield col[i]


def sw_diag(lol):
    l = len(lol)
    for i, col in enumerate(columns(lol)):
        yield col[l - i - 1]


def is_magic_square(lol):
    if len(lol) == 0:
        return False

    s = sum(lol[0])

    for row in lol[1:]:
        if sum(row) != s:
            return False
    for col in columns(lol):
        if sum(col) != s:
            return False

    if sum(se_diag(lol)) != s:
        return False

    if sum(sw_diag(lol)) != s:
        return False

    return True


def all_squares(order):
    size = order * order
    progress = progressbar.ProgressBar(max_value=math.factorial(size))

    def wat():
        for values in itertools.permutations([1 + v for v in range(size)]):
            yield [values[n:n + order] for n in range(0, size, order)]

    return progress(wat())


if __name__ == "__main__":
    import pprint

    try:
        for sq in all_squares(3):
            if is_magic_square(sq):
                pprint.pprint(sq)
                break
    except KeyboardInterrupt:
        pass
