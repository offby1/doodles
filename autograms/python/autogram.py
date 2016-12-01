import collections
import random
import string

import num2words                # pip install num2words
import progressbar              # pip install progressbar2

ALPHABET = string.ascii_lowercase

def number_and_letter_to_string(number, letter):
    return "{} '{}'{}".format(num2words.num2words(number), letter, '' if number == 1 else "s")


def join_strings(strings):
    most = strings[:-1]
    return ', '.join(most) + ' and ' + strings[-1]


def new_counter(c):
    terse = [number_and_letter_to_string(v, k) for k, v in sorted(c.items()) if k.isalpha()]
    string = "Prince Phillip and Queen Elizabeth keep " + join_strings(terse) + ' at Buckingham Palace.'

    return collections.Counter(string.lower()), string


class Seen:
    def __init__(self):
        self.seen = set()

    def __call__(self, counter):
        return self.freeze(counter) in self.seen

    def note(self, counter):
        self.seen.add(self.freeze(counter))

    def freeze(self, c):
        return tuple(c.items())

    def len(self):
        return len(self.seen)


def perturb_counter(c, seen):
    while seen(c):
        key = random.choice(ALPHABET)
        value = c[key]
        c[key] = value + random.randrange(-3, 4)


def chase_string(string):
    seen_counters = Seen()

    c = collections.Counter(string)
    last = None
    finished_naturally = False

    with progressbar.ProgressBar(max_value=progressbar.UnknownLength) as bar:
        try:
            while True:
                if seen_counters(c):
                    if c == last:
                        finished_naturally = True
                        break
                    else:
                        bar.update(seen_counters.len())
                        perturb_counter(c, seen_counters)

                seen_counters.note(c)

                last = c
                c, string = new_counter(c)
        except KeyboardInterrupt:
            pass

    print('{} {}'.format('!!' if finished_naturally else '?', string))

chase_string(ALPHABET)

# This text contains twelve i's, seven h's, four a's, four x's, thirty-two e's, seven v's, one z, one j, eighteen n's, three d's, five y's, sixteen o's, five u's, twenty-eight s's, six w's, one q, four g's, one p, twenty t's, seven f's, four l's, one m, two c's, eight r's, one k and one b.  It really does!

# This text contains four a's, one b, two c's, three d's, thirty-two e's, seven f's, four g's, seven h's, twelve i's, one j, one k, four l's, one m, eighteen n's, sixteen o's, one p, one q, eight r's, twenty-eight s's, twenty t's, five u's, seven v's, six w's, four x's, five y's and one z.  It really does!

# Prince Phillip and Queen Elizabeth keep eight 'a's, three 'b's, four 'c's, three 'd's, thirty-five 'e's, nine 'f's, four 'g's, thirteen 'h's, twenty-one 'i's, one 'j', three 'k's, six 'l's, two 'm's, fifteen 'n's, eight 'o's, six 'p's, two 'q's, eleven 'r's, thirty 's's, twenty-three 't's, five 'u's, six 'v's, six 'w's, five 'x's, five 'y's and two 'z's at Buckingham Palace.
