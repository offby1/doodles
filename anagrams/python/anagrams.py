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


def anagrams (bag, exclusions, debug_level):

    # We'll add stuff to exclusions as we go through our loop, but we
    # don't want those changes to affect our caller.  So we deep-copy
    # exclusions, and modify the copy.
    exclusions = exclusions.copy ()

    rv = []

    for entry in the_dict_list:
        key   = entry[0]
        words = entry[1]

        if (exclusions.has_key (key)):
            continue

        smaller_bag = subtract_bags (bag, key)
        if (not smaller_bag):
            continue

        if (bag_empty (smaller_bag)):
            for w in words:
                rv.append ([w])
                if (not debug_level):
                    print w
        else:
            from_smaller_bag = anagrams (smaller_bag,
                                         exclusions,
                                         debug_level + 1)
            if (not len (from_smaller_bag)):
                continue

            for new in combine (words, from_smaller_bag):
                rv.append (new)
                if (not debug_level):
                    print new

        exclusions[key] = 1

    assert (is_list_of_anagrams (rv))
    return rv

dict_fn = "/usr/share/dict/words"
print "Snarfing", dict_fn
dict_hash_table = snarf_dictionary (dict_fn)
print "done"

the_phrase = bag (sys.argv[1])
print "Pruning dictionary.  Before:", len (dict_hash_table.keys ())

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

print "Pruned dictionary.  After:", len (the_dict_list)
result = anagrams (the_phrase, {}, 0)
print len(result), "anagrams of", sys.argv[1], ":"

for a in result:
    sys.stdout.write ("(")
    for i, w in  enumerate (a):
        if (i):
            sys.stdout.write (" ")
        sys.stdout.write  (w)
    sys.stdout.write (")")
    print
    

