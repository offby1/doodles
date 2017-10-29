# https://fivethirtyeight.com/features/how-long-will-it-take-to-blow-out-the-birthday-candles/

import random
import statistics


def trial(number_burning=30):
    blows = 0
    while (number_burning > 0):
        n = random.randint(1, number_burning)
        number_burning -= n

        blows += 1

    return blows


data = [trial() for _ in range(10000)]
print ('Mean : {}'.format(statistics.mean(data)))
print ('Stdev: {}'.format(statistics.stdev(data)))
