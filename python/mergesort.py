import random

from hypothesis import given, strategies as st


def mergesort(seq, key=None):
    if key is None:
        key = lambda x: x

    l_ = len(seq)

    if l_ < 2:
        return seq

    half_length = l_ // 2

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


@given(st.lists(st.integers()))
def test_em_all_let_God_sort_em_out(original):
    actual = list(mergesort(original))
    expected = sorted(original)

    assert actual == expected
