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
import string
import sys

# 3rd-party
import num2words                # pip install num2words

def survey_sentence(s):
    return collections.Counter(filter(str.isalpha, str(s).lower()))

def survey_to_string(s, template):
    return template.format(**{letter: num2words.num2words(number) for letter, number in s.items()})

lc_letters = sorted(set(string.letters.lower()))
default_values = {letter: 0 for letter in lc_letters}

def template_snippet(l):
    return "{%s} '%s's" % (l, l)

all_snippets = [template_snippet(l) for l in lc_letters[:5]]

snippets_plus_and = all_snippets
snippets_plus_and.insert(-1, 'and')
template = "This sentence has {}.".format(', '.join(snippets_plus_and))


old = survey_to_string(survey_sentence(template.format(**default_values)), template)

while True:
    new = survey_to_string(survey_sentence(old), template)

    if old==new:
        print(new)
        break
    print('.', file=sys.stderr, end='')
    old = new
