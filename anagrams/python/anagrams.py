#!/usr/bin/env python

from bag import bag, bag_empty, bags_equal, subtract_bags
from dict import snarf_dictionary
from types import *
import sys

## These are just for internal sanity checking.
def is_anagram (thing):
    assert type(thing) is ListType
    for w in thing:
        assert type(w) is StringType
    return 1
    
def is_list_of_anagrams (thing):
    assert type(thing) is ListType
    for a in thing:
        assert is_anagram(a)
    return 1
## End of sanity checking stuff.

def combine (words, anagrams):
    assert (is_anagram (words))
    assert (is_list_of_anagrams (anagrams))

    rv = []
    for w in words:
        for a in anagrams:
            rv.append ([w] + a)

    assert (is_list_of_anagrams (rv))
    assert (len (rv) == len (anagrams) * len (words))

    return rv


def anagrams (bag, dict):

    rv = []

    for words_processed in range (0, len (dict)):
        entry = dict[words_processed]
        key   = entry[0]
        words = entry[1]

        smaller_bag = subtract_bags (bag, key)
        if (not smaller_bag):
            continue

        if (bag_empty (smaller_bag)):
            for w in words:
                rv.append ([w])
        else:
            from_smaller_bag = anagrams (smaller_bag,
                                         dict[words_processed + 1:])
            if (not len (from_smaller_bag)):
                continue

            for new in combine (words, from_smaller_bag):
                rv.append (new)

    assert (is_list_of_anagrams (rv))
    return rv

dict_fn = "/usr/share/dict/words"
print >> sys.stderr, "Snarfing", dict_fn
dict_hash_table = snarf_dictionary (dict_fn)
print >> sys.stderr, "done"

the_phrase = bag (sys.argv[1])
print >> sys.stderr, "Pruning dictionary.  Before:", len (dict_hash_table.keys ())

# Now convert the hash table to a list, longest entries first.  (This
# isn't necessary, but it makes the more interesting anagrams appear
# first.)  While we're at it, prune the list, too.
the_dict_list = []
for k in dict_hash_table.keys ():
    if (subtract_bags (the_phrase, k)):
        the_dict_list.append([k, dict_hash_table[k]])

# Note that sorting entries "alphabetically" only makes partial sense,
# since each entry is (at least potentially) more than one word (all
# the words in an entry are anagrams of each other).
def biggest_first_then_alphabetically (a, b):
    a = a[1][0]
    b = b[1][0]
    result = cmp (len (b), len (a))
    if (not result):
        result = cmp (a, b)
    return result

the_dict_list.sort (biggest_first_then_alphabetically)

print >> sys.stderr, "Pruned dictionary.  After:", len (the_dict_list)
result = anagrams (the_phrase, the_dict_list)
print >> sys.stderr, len(result), "anagrams of", sys.argv[1], ":"

for a in result:
    sys.stdout.write ("(")
    for i, w in  enumerate (a):
        if (i):
            sys.stdout.write (" ")
        sys.stdout.write  (w)
    sys.stdout.write (")")
    print
