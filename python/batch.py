# from http://stackoverflow.com/a/8290514/20146
from itertools import islice, chain


def batch(iterable, size):
    sourceiter = iter(iterable)
    while True:
        batchiter = islice(sourceiter, size)
        yield tuple(chain([next(batchiter)], batchiter))


def batch2(iterable, size):
    for i in range(0, len(iterable), size):
        yield tuple(iterable[i:i + size])


print(list(batch(range(10), 3)))
print(list(batch2(range(10), 3)))
