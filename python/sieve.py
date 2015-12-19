candidates = [True] * 10000

for factor in range(2, len(candidates)):
    for x in range(factor * 2, len(candidates), factor):
        candidates[x] = False

primes = [index for index, is_prime in enumerate(candidates) if is_prime]

print(primes)
