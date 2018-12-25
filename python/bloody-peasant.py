#!/usr/bin/env python3

import collections
import pprint

factors_by_product = collections.defaultdict(set)


def product(left, right):
    frozen = tuple(sorted((right, left)))

    prod = 0
    while left > 0:
        if left % 2:
            prod = prod + right
        left = left // 2
        right *= 2

    if frozen[0] > 1:
        factors_by_product[prod].add(frozen)

    return prod


for x in range (10):
    for y in range(10):
        product(x, y)

pprint.pprint(dict(factors_by_product))
