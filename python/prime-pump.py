# http://bit-player.org/2015/pumping-the-primes

import math


def lotta_primes():
    n = 2
    a = 7
    while True:
        g = math.gcd(n, a)
        n = n + 1
        a = a + g
        if g != 1:
            yield g


found = set()
for p in lotta_primes():
    if p not in found:
        print(p)
    found.add(p)
    if len(found) > 50:
        break
