import random


def mergesort(seq, key=None):
    if key is None:
        key = lambda x: x

    l = len(seq)

    if l < 2:
        return seq

    half_length = l // 2

    left = seq[0:half_length]
    right = seq[half_length:]

    return list(merged(mergesort(left, key=key),
                       mergesort(right, key=key),
                       key=key))


def merged(s1, s2, key):
    while len(s1) + len(s2):
        if len(s2) == 0:
            yield s1.pop(0)
        elif len(s1) == 0:
            yield s2.pop(0)
        elif key(s1[0]) <= key(s2[0]):  # <= as opposed to < makes it stable
            yield s1.pop(0)
        else:
            yield s2.pop(0)


original = [random.randint(0, 100) for _ in range(100)]

sorted = list(mergesort(original))

print(sorted)
