#!/usr/bin/env python

"""
Generate an "autogram" -- https://en.wikipedia.org/wiki/Autogram
"""

from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

__author__ = 'ehanchrow@ine.com'

# Core
import collections
import random
import string

# 3rd-party
import num2words                # pip install num2words


lc_letters = sorted(set(string.letters.lower()))


def string_to_counter(s):
    return collections.Counter(filter(str.isalpha, str(s).lower()))


def perturb_counter(c):
    for key in c:
        c[key] = max(c[key] + random.randrange(-10, 10), 0)
    return c


def counter_to_string(c, template):
    def pluralize(string, number):
        if number == 1:
            return string
        return string + "'s"

    def slot(letter, number):
        return '{} {}'.format(num2words.num2words(number), pluralize(letter, number))

    return template.format(**{letter: slot(letter, c.get(letter, 0)) for letter in lc_letters})


def template_snippet(l):
    return "{{{}}}".format(l)

if __name__ == "__main__":
    all_snippets = [template_snippet(l) for l in lc_letters[:18]]

    initial_counter = {letter: 0 for letter in lc_letters}

    snippets_plus_and = all_snippets
    snippets_plus_and[-1] = 'and ' + snippets_plus_and[-1]
    template = "This sentence, over which I slaved and struggled for many years, has {}.".format(', '.join(snippets_plus_and))

    old = counter_to_string(string_to_counter(template.format(**initial_counter)), template)

    seen = set([old])

    while True:
        survey = string_to_counter(old)
        new = counter_to_string(survey, template)

        if old == new:
            print(len(seen))
            print(new)
            break

        while new in seen:
            survey = perturb_counter(survey)
            new = counter_to_string(survey, template)

        seen.add(new)
        if len(seen) % 1000 == 0:
            print(len(seen))

        old = new
