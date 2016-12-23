import math
import random

import progressbar              # pip install progressbar2


def columns(lol):
    for c in range(len(lol[0])):
        yield [row[c] for row in lol]


def se_diag(lol):
    for i, col in enumerate(columns(lol)):
        yield col[i]


def sw_diag(lol):
    for i, col in enumerate(reversed(list(columns(lol)))):
        yield col[i]


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

_3x3 = [
    [2, 7, 6],
    [9, 5, 1],
    [4, 3, 8]
]

def random_magic_square(order):
    values = [1 + v for v in range(order * order)]
    random.shuffle(values)

    rv = []
    for n in range(0, order * order, order):
        rv.append(values[n:n + order])

    return rv

if __name__ == "__main__":
    import json
    import pprint

    order = 3
    bar = progressbar.ProgressBar(max_value=math.factorial(order * order))
    bar.start()
    trials = 0
    while True:
        ms = random_magic_square(order)
        bar.update(trials)
        if is_magic_square(ms):
            pprint.pprint(ms)
            exit(0)

        trials += 1
