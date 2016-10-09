# from http://stackoverflow.com/a/8290514/20146
from itertools import islice, chain


def batch(iterable, size):
    sourceiter = iter(iterable)
    while True:
        batchiter = islice(sourceiter, size)
        yield tuple(chain([next(batchiter)], batchiter))

print(list(batch(range(10), 3)))
