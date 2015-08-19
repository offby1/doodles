# http://bit-player.org/2015/pumping-the-primes

import fractions


def lotta_primes():
    n = 2
    a = 7
    while True:
        g = fractions.gcd(n, a)
        n = n + 1
        a = a + g
        if g != 1:
            yield g

for i, p in enumerate(lotta_primes()):
    print(p)
    if i > 10:
        break
