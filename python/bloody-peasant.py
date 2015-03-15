#!/usr/bin/env python

"""Some documentation would be nice!"""

from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

__author__ = 'ehanchrow@ine.com'

import collections

factors_by_product = collections.defaultdict(set)

def product(left, right):
    frozen = tuple(sorted((right, left)))

    prod = 0
    while left > 0:
        if left % 2:
            prod = prod + right
        left = left / 2
        right *= 2

    if frozen[0] > 1:
        factors_by_product[prod].add(frozen)

    return prod

for x in range (10):
    for y in range(10):
        product(x, y)

import pprint
pprint.pprint(dict(factors_by_product))
