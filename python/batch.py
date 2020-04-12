# from http://stackoverflow.com/a/8290514/20146
def batch(iterable, size):
    for i in range(0, len(iterable), size):
        yield tuple(iterable[i:i + size])

print(list(batch(range(10), 3)))
