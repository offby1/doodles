"""Illustrate
http://www.billthelizard.com/2009/09/getting-fair-toss-from-biased-coin.html

"""

import collections
import pprint
import random


def biased_coinflip():
    return random.Random().uniform(0, 1) < .01


def make_unbiased_flipper(biased_flipper):

    def unbiased_flip():
        while True:
            first  = biased_coinflip()
            second = biased_coinflip()

            if first != second:
                return first

    return unbiased_flip


def run_trials(flipper, num=100):
    histogram = collections.defaultdict(int)
    for t in range(num):
        histogram[flipper()] += 1

    print('{:>15}: {}'.format(flipper.func_name, pprint.pformat(dict(histogram))))

run_trials(biased_coinflip)
run_trials(make_unbiased_flipper(biased_coinflip))
