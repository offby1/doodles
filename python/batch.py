import itertools


# from http://stackoverflow.com/a/8290514/20146
def batch(sequence, size):
    for i in range(0, len(sequence), size):
        yield tuple(sequence[i : i + size])


# made this one up myself
def b2(iterable, size):
    i = iter(iterable)
    while True:
        one_batch = []
        while len(one_batch) < size:
            try:
                one_batch.append(next(i))
            except StopIteration:
                if one_batch:
                    yield one_batch
                return

        if one_batch:
            yield one_batch


print(list(batch(range(10), 3)))
print(list(b2(range(10), 3)))


try:
    print(list(itertools.islice(batch(itertools.count(), 5), 3)))
except Exception as e:
    print(f"Oh noes: {e}")
print(list(itertools.islice(b2(itertools.count(), 5), 3)))
