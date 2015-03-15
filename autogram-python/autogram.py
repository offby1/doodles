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

def survey_sentence(s):
    return collections.Counter(filter(str.isalpha, str(s).lower()))


def perturb_survey(s):
    for key in s:
        s[key] = max(s[key] + random.randrange(-10, 10), 0)
    return s


def survey_to_string(s, template):
    def pluralize(string, number):
        if number == 1:
            return string
        return string + "'s"

    def slot(letter, number):
        return '{} {}'.format(num2words.num2words(number), pluralize(letter, number))

    return template.format(**{letter: slot(letter, s.get(letter, 0)) for letter in lc_letters})


def template_snippet(l):
    return "{%s}" % (l, )

if __name__ == "__main__":
    all_snippets = [template_snippet(l) for l in lc_letters[:10]]

    default_values = {letter: 0 for letter in lc_letters}

    snippets_plus_and = all_snippets
    snippets_plus_and[-1] = 'and ' + snippets_plus_and[-1]
    template = "This sentence has {}.".format(', '.join(snippets_plus_and))

    old = survey_to_string(survey_sentence(template.format(**default_values)), template)

    seen = set([old])

    while True:
        survey = survey_sentence(old)
        new = survey_to_string(survey, template)

        if old == new:
            print(new)
            break

        while new in seen:
            survey = perturb_survey(survey)
            new = survey_to_string(survey, template)

        seen.add(new)
        if len(seen) % 1000 == 0:
            print(len(seen))

        old = new
