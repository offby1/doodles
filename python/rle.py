import dataclasses
import operator
import random

@dataclasses.dataclass
class Streak:
    starting_index: int
    length: int
    item: object


def run_length_encode(seq):
    current_streak = None

    for index, elt in enumerate(seq):
        if current_streak is None:
            current_streak = Streak(index, 1, elt)
        elif elt != current_streak.item:
            yield current_streak
            current_streak = Streak(index, 1, elt)
        else:
            current_streak.length += 1

    if current_streak is not None:
        yield current_streak


flips = ''.join(random.choices("HT", k=100))
print(flips)
longest_streak = max(run_length_encode(flips), key=operator.attrgetter('length'))
print(' ' * longest_streak.starting_index, end='^\n')
print(longest_streak)
