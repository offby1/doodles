#!/usr/bin/env python

from bag import bag, bag_empty, bags_equal, subtract_bags
from dict import snarf_dictionary
from types import *
import sys

dict_fn = "/usr/share/dict/words"
print "Snarfing", dict_fn
the_dict = snarf_dictionary (dict_fn)
print "done"

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

def excluded (bag, exclusions):
    rv = exclusions.has_key (bag)
    return rv

def combine (words, anagrams):
    rv = []
    assert (is_anagram (words))
    assert (is_list_of_anagrams (anagrams))
    for w in words:
        for a in anagrams:
            rv.append ([w] + a)

    assert (is_list_of_anagrams (rv))

    assert (len (rv) == len (anagrams) * len (words))
    return rv


def anagrams (bag, exclusions, debug_level):
    exclusions = exclusions.copy ()
    rv = []

    for key in the_dict.keys ():
        words = the_dict[key]

        if (excluded (key, exclusions)):
            continue

        smaller_bag = subtract_bags (bag, key)
        if (0 == smaller_bag):
            continue

        if (bag_empty (smaller_bag)):
            exclusions[key] = 1
            for w in words:
                rv.append ([w])
        else:
            from_smaller_bag = anagrams (smaller_bag,
                                         exclusions,
                                         debug_level + 1)
            if (0 == len (from_smaller_bag)):
                continue
            exclusions[key] = 1

            combined = combine (words, from_smaller_bag)
            for new in combined:
                rv.append (new)


    assert (is_list_of_anagrams (rv))
    return rv

#print combine (["some", "new", "words"], [["annie", "grams"], ["grams", "annie"]])

the_phrase = bag (sys.argv[1])
print "Pruning dictionary.  Before:", len (the_dict.keys ())

for k in the_dict.keys ():
    if (0 == subtract_bags (the_phrase, k)):
        del the_dict[k]

print "Pruned dictionary.  After:", len (the_dict.keys ())
result = anagrams (the_phrase, {}, 0)
print len(result), "anagrams of", sys.argv[1], result, ":",

for a in result:
    sys.stdout.write ("(")
    for i, w in  enumerate (a):
        if (i):
            sys.stdout.write (" ")
        sys.stdout.write  (w)
    sys.stdout.write (")")
    print
    

