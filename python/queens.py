#!/usr/bin/env python

"""
The traditional 8-queens problem, solved with brute force.
"""

from __future__ import print_function
from __future__ import absolute_import

__author__ = 'eric.hanchrow@gmail.com'

import itertools

class Board(object):
    def __init__(self, rows=8, columns=8):
        self.rows = rows
        self.columns = columns
        self.unattacked = set(itertools.product(range(8), range(8)))
        self.occupied = set()

    def star(self, sq):
        row  = map(lambda c: (sq[0], c), range(8))
        col  = map(lambda r: (r, sq[1]), range(8))
        nesw = map(lambda i: (sq[0] + i, sq[1] + i), range(-8, 8))
        nwse = map(lambda i: (sq[0] + i, sq[1] - i), range(-8, 8))
        for candidate in sorted(set(itertools.chain(row, col, nesw, nwse))):
            if candidate[0] >= 0 and candidate[0] < self.columns:
                if candidate[1] >= 0 and candidate[1] < self.rows:
                    yield candidate

    def place_queen(self, sq):
        new = Board()
        new.occupied = set(self.occupied)
        new.occupied.add(sq)

        for s in self.star(sq):
            new.unattacked.discard(s)

        return new

    def __str__(self):
        return "Board with queens at {}".format(list(self.occupied))


def outer(outer_n):

    def solve(n):
        if n == 0:
            yield Board()

        else:
            for subsolution in solve(n - 1):
                for sq in subsolution.unattacked:
                    yield subsolution.place_queen(sq)

    return [b for b in solve(outer_n) if len(b.occupied) == outer_n]

for s in outer(3):
    print(s)
