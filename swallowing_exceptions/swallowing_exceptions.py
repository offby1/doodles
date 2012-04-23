#!/usr/bin/env python

"""
Demonstrate a cute but perhaps pointless thingy
"swallowing_exceptions".  It basically lets you write a simple
exception handler in one line instead of three.
"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

__author__ = 'eric.hanchrow@gmail.com'

import contextlib

@contextlib.contextmanager
def swallowing_exceptions(exn_class):
    try:
        yield
    except exn_class, e:
        print("Swallowing an exception!", str(e))

class BigFatException(Exception):
    pass

def process(datum):
    if datum %2 == 0:
        raise BigFatException("I don't like even numbers")
    return "{0} is nice because it's not even".format(datum)

data = (1, 2, 3, 4, 5)

for d in data:
    with swallowing_exceptions(BigFatException):
        print(process(d))
        raise BigFatException, "I am swallowed!"
    
