#!/usr/bin/env python3

"""The traditional 8-queens problem, copied from
https://hg.python.org/cpython/file/3.5/Lib/test/test_generators.py

"""

# A conjoin-based N-Queens solver.


def conjoin(gs):

    n = len(gs)
    values = [None] * n

    # Do one loop nest at time recursively, until the # of loop nests
    # remaining is divisible by 3.

    def gen(i):
        if i >= n:
            yield values

        elif (n - i) % 3:
            ip1 = i + 1
            for values[i] in gs[i]():
                for x in gen(ip1):
                    yield x

        else:
            for x in _gen3(i):
                yield x

    # Do three loop nests at a time, recursing only if at least three more
    # remain.  Don't call directly:  this is an internal optimization for
    # gen's use.

    def _gen3(i):
        assert i < n and (n - i) % 3 == 0
        ip1, ip2, ip3 = i + 1, i + 2, i + 3
        g, g1, g2 = gs[i:ip3]

        if ip3 >= n:
            # These are the last three, so we can yield values directly.
            for values[i] in g():
                for values[ip1] in g1():
                    for values[ip2] in g2():
                        yield values

        else:
            # At least 6 loop nests remain; peel off 3 and recurse for the
            # rest.
            for values[i] in g():
                for values[ip1] in g1():
                    for values[ip2] in g2():
                        for x in _gen3(ip3):
                            yield x

    for x in gen(0):
        yield x


class Queens:
    def __init__(self, n):
        self.n = n
        rangen = range(n)

        # Assign a unique int to each column and diagonal.
        # columns:  n of those, range(n).
        # NW-SE diagonals: 2n-1 of these, i-j unique and invariant along
        # each, smallest i-j is 0-(n-1) = 1-n, so add n-1 to shift to 0-
        # based.
        # NE-SW diagonals: 2n-1 of these, i+j unique and invariant along
        # each, smallest i+j is 0, largest is 2n-2.

        # For each square, compute a bit vector of the columns and
        # diagonals it covers, and for each row compute a function that
        # generates the possiblities for the columns in that row.
        self.rowgenerators = []
        for i in rangen:
            rowuses = [
                (1 << j)
                | (1 << (n + i - j + n - 1))  # column ordinal
                | (1 << (n + 2 * n - 1 + i + j))  # NW-SE ordinal  # NE-SW ordinal
                for j in rangen
            ]

            def rowgen(rowuses=rowuses):
                for j in rangen:
                    uses = rowuses[j]
                    if uses & self.used == 0:
                        self.used |= uses
                        yield j
                        self.used &= ~uses

            self.rowgenerators.append(rowgen)

    # Generate solutions.
    def solve(self):
        self.used = 0
        for row2col in conjoin(self.rowgenerators):
            yield row2col

    def printsolution(self, row2col):
        n = self.n
        assert n == len(row2col)
        sep = "+" + "-+" * n
        print(sep)
        for i in range(n):
            squares = [" " for j in range(n)]
            squares[row2col[i]] = "Q"
            print("|" + "|".join(squares) + "|")
            print(sep)


q = Queens(8)

for solution in q.solve():
    q.printsolution(solution)
