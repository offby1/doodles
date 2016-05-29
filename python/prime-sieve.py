def sieve(ints):
    prime = next(ints)
    yield prime
    yield from sieve(i for i in ints if i % prime)

for i in sieve(iter(range(2, 30000))):
    print(i)
