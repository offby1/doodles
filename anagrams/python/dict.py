#!/usr/bin/env python

import StringIO
import string
import re
from bag import bag, bag_empty, bags_equal, subtract_bags

has_a_vowel_re = re.compile (r'[aeiou]')
long_enough_re = re.compile (r'^i$|^a$|^..')
non_letter_re = re.compile (r'[^a-zA-Z]')

def snarf_dictionary_from_IO (I):
    all_words = re.findall (r'.+', I.read ())
    rv = {}
    for w in all_words:
        w = string.lower (w)
        if non_letter_re.search (w):
            continue
        if (not long_enough_re.match (w)):
            continue
        if (not has_a_vowel_re.search (w)):
            continue

        key = bag(w)
        if rv.has_key (key):
            if (0 == rv[key].count (w)): # avoid duplicates
                rv[key].append (w)
        else:
            rv[key] = [w]

    return rv

def snarf_dictionary (fn):
    fh = open (fn, "r")
    rv = snarf_dictionary_from_IO (fh)
    fh.close ()
    return rv

fake_input = "cat\ntac\nfred\n"
fake_dict = snarf_dictionary_from_IO (StringIO.StringIO (fake_input))

assert (2 == len (fake_dict.keys ()))
cat_hits = fake_dict[bag ("cat")]
assert (2 == len (cat_hits))
assert (cat_hits[0] == "cat")
assert (cat_hits[1] == "tac")
assert (1 == len (fake_dict[bag ("fred")]))
assert (fake_dict[bag ("fred")][0] == "fred")

print __name__, "tests passed."
