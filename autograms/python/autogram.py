import collections
import functools
import random
import string

import num2words                # pip install num2words
import tqdm                     # pip install tqdm

ALPHABET = string.ascii_lowercase


@functools.lru_cache(maxsize=128, typed=False)
def n2words(number):
    return num2words.num2words(number)


def number_and_letter_to_string(number, letter):
    return "{} '{}'{}".format(n2words(number), letter, '' if number == 1 else "s")


def join_strings(strings):
    most = strings[:-1]
    return ', '.join(most) + ' and ' + strings[-1]


def new_counter(c, letters_to_count):
    terse = [number_and_letter_to_string(v, k) for k, v in c.sorted_items() if k.isalpha()]
    string = "Prince Phillip and Queen Elizabeth keep " + \
        join_strings(terse[0:letters_to_count]) + ' at Buckingham Palace.'

    return HashableCounter(string.lower()), string


class HashableCounter(collections.Counter):

    def sorted_items(self):
        return sorted(self.items())

    def __hash__(self):
        return hash(tuple(self.sorted_items()))


def generate_new_counter(c, seen):
    while c in seen:
        key = random.choice(ALPHABET)
        value = c[key]
        c[key] = value + random.randrange(-3, 4)


def chase_string(string, letters_to_count):
    seen_counters = set()

    c = HashableCounter(string)
    last = None
    finished_naturally = False

    with tqdm.tqdm() as bar:
        try:
            while True:
                if c in seen_counters:
                    if c == last:
                        # We found a sentence that maps to itself --
                        # i.e., it's "true".  Hooray!
                        finished_naturally = True
                        break
                    else:
                        # Displaying the progress bar is surprisingly
                        # slow, so let's not do that too often.
                        if (len(seen_counters) % 1000) == 0:
                            bar.update(len(seen_counters))
                        generate_new_counter(c, seen_counters)

                seen_counters.add(c)

                last = c
                c, string = new_counter(c, letters_to_count)
        except KeyboardInterrupt:
            pass

    print('{} {}'.format('!!' if finished_naturally else '?', string))
    return finished_naturally


if __name__ == "__main__":
    random.seed(0)
    for letters_to_count in range(1, len(ALPHABET) + 1):
        chase_string(ALPHABET, letters_to_count)

# This text contains twelve i's, seven h's, four a's, four x's, thirty-two
# e's, seven v's, one z, one j, eighteen n's, three d's, five y's, sixteen
# o's, five u's, twenty-eight s's, six w's, one q, four g's, one p, twenty
# t's, seven f's, four l's, one m, two c's, eight r's, one k and one b.
# It really does!

# This text contains four a's, one b, two c's, three d's, thirty-two e's,
# seven f's, four g's, seven h's, twelve i's, one j, one k, four l's, one
# m, eighteen n's, sixteen o's, one p, one q, eight r's, twenty-eight s's,
# twenty t's, five u's, seven v's, six w's, four x's, five y's and one z.
# It really does!

# Prince Phillip and Queen Elizabeth keep eight 'a's, three 'b's, four
# 'c's, three 'd's, thirty-five 'e's, nine 'f's, four 'g's, thirteen 'h's,
# twenty-one 'i's, one 'j', three 'k's, six 'l's, two 'm's, fifteen 'n's,
# eight 'o's, six 'p's, two 'q's, eleven 'r's, thirty 's's, twenty-three
# 't's, five 'u's, six 'v's, six 'w's, five 'x's, five 'y's and two 'z's
# at Buckingham Palace.
