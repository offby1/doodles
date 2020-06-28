#!/usr/bin/env python3

"""This is the one use case I know of for which concurrency is
obviously a win: We will download a pile of web pages, and find that
doing so concurrently is faster than doing them in sequence.

We also do the concurrent downloads a bunch of different times, using
different techniques.  tl;dr -- they're all about the same speed.

"""
import timeit

import asynchronous  # noqa
import future  # noqa
import sequential  # noqa
import threaded  # noqa
import treeoh  # noqa

# We'll download all these URLS, either one at a time, or in parallel.

# I have no idea why I chose wikipedia articles with "Eagle" in their
# titles.  Just go with it.
urls = (
    'https://en.wikipedia.org/wiki/American_Eagles',
    'https://en.wikipedia.org/wiki/Ateneo_Blue_Eagles',
    'https://en.wikipedia.org/wiki/Bedford_Town_F.C.',
    'https://en.wikipedia.org/wiki/Boston_College_Eagles',
    'https://en.wikipedia.org/wiki/Chris_Eagles',
    'https://en.wikipedia.org/wiki/Colorado_Eagles',
    'https://en.wikipedia.org/wiki/Coppin_State_Eagles',
    'https://en.wikipedia.org/wiki/Crystal_Palace_F.C.',
    'https://en.wikipedia.org/wiki/Eagle_(disambiguation)',
    'https://en.wikipedia.org/wiki/Eagles_(1984_film)',
    'https://en.wikipedia.org/wiki/Eagles_(2012_film)',
    'https://en.wikipedia.org/wiki/Eagles_(album)',
    'https://en.wikipedia.org/wiki/Eagles_(band)',
    'https://en.wikipedia.org/wiki/Eagles_(box_set)',
    'https://en.wikipedia.org/wiki/Eagles_cricket_team',
    'https://en.wikipedia.org/wiki/Eastern_Michigan_Eagles',
    'https://en.wikipedia.org/wiki/Eastern_Washington_Eagles',
    'https://en.wikipedia.org/wiki/Embry%E2%80%93Riddle_Aeronautical_University',
    'https://en.wikipedia.org/wiki/Fraternal_Order_of_Eagles',
    'https://en.wikipedia.org/wiki/Georgia_Southern_Eagles',
    'https://en.wikipedia.org/wiki/Germany_national_football_team',
    'https://en.wikipedia.org/wiki/Greg_Eagles',
    # 'https://en.wikipedia.org/wiki/Hanwha_Eagles',
    # 'https://en.wikipedia.org/wiki/Jeanne_Eagels',
    # 'https://en.wikipedia.org/wiki/Manly-Warringah_Sea_Eagles',
    # 'https://en.wikipedia.org/wiki/Marquette_Golden_Eagles',
    # 'https://en.wikipedia.org/wiki/Morehead_State_Eagles',
    # 'https://en.wikipedia.org/wiki/Niagara_Purple_Eagles',
    # 'https://en.wikipedia.org/wiki/North_Texas_Eagles',
    # 'https://en.wikipedia.org/wiki/Northern_Eagles',
    # 'https://en.wikipedia.org/wiki/Oral_Roberts_Golden_Eagles',
    # 'https://en.wikipedia.org/wiki/P.A.O.K.',
    # 'https://en.wikipedia.org/wiki/PFC_Ludogorets_Razgrad',
    # 'https://en.wikipedia.org/wiki/Philadelphia_Eagles',
    # 'https://en.wikipedia.org/wiki/S.L._Benfica',
    # 'https://en.wikipedia.org/wiki/SWD_Eagles',
    # 'https://en.wikipedia.org/wiki/Sheffield_Eagles',
    # 'https://en.wikipedia.org/wiki/Southern_Miss_Golden_Eagles',
    # 'https://en.wikipedia.org/wiki/Surrey_Eagles',
    # 'https://en.wikipedia.org/wiki/The_Eagles_(UK_band)',
    # 'https://en.wikipedia.org/wiki/The_Eagles_(rhythm_and_blues_group)',
    # 'https://en.wikipedia.org/wiki/Tohoku_Rakuten_Golden_Eagles',
    # 'https://en.wikipedia.org/wiki/USA_Eagles',
    # 'https://en.wikipedia.org/wiki/USCGC_Eagle_(WIX-327)',
    # 'https://en.wikipedia.org/wiki/Washington_Eagles',
    # 'https://en.wikipedia.org/wiki/West_Coast_Eagles',
    # 'https://en.wikipedia.org/wiki/Winthrop_Eagles',
    # 'https://en.wikipedia.org/wiki/Woodville-West_Torrens_Eagles',
)

# urls=urls[0:3]


def t(description, python_expression):
    print()
    print(f'{description}: starting')
    time_in_seconds = timeit.timeit(python_expression, globals=globals(), number=1)
    number_of_requests = len(urls)
    print(f'Downloading {number_of_requests} urls with {description!r} took {time_in_seconds} seconds: {number_of_requests / time_in_seconds} requests per second')
    print(f'{description} done')
    print()


for module_name in ("sequential", "threaded", "future", "treeoh", "asynchronous"):
    t(module_name, f'{module_name}.download(urls)')
