# http://fivethirtyeight.com/features/can-you-deal-with-these-card-game-puzzles/

import random


def trial():
    deck = list(range(52))
    random.shuffle(deck)
    for i, c in enumerate(deck):
        if c == i % 4:
            return False

    return True


if __name__ == "__main__":
    print (sum([trial() for t in range(10000)]))
