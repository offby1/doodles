#!/usr/bin/env python

"""Some documentation would be nice!"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

__author__ = 'erich@cozi.com'

def key_schedule(key):
    key = map(ord, key)

    s = range(256)

    j = 0
    for i, _ in enumerate(s):
        ki = key[i % len(key)]

        j = (j + s[i] + ki) % 256
        print ("Swapping indices s[{i}] ({0}) and s[{j}] ({1})".format(s[i],
                                                                       s[j],
                                                                       **locals()))
        s[i], s[j] = s[j], s[i]

    return s

print(key_schedule(b"x"))
