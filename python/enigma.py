#!/usr/bin/env python3
import random
import string
import sys

alphabet = string.ascii_letters + string.digits + string.punctuation


def rotate(l):
    l.append (l.pop (0))


class Wheel:
    def __init__(self, num_slots):
        self.offset = 0
        self.permutation = list(range(num_slots))
        random.shuffle(self.permutation)

    def advance(self):
        rotate(self.permutation)

        self.offset += 1

        if self.offset == len(self.permutation):
            self.offset = 0
            return True
        return False

    def transform(self, number, encrypt):
        return self.permutation[number] if encrypt else self.permutation.index(number)

    def reflect(self, number):
        n = len(self.permutation)
        offset = n // 2
        return (number + offset) % n


def to_numbers(str):
    nums = []
    for letter in str:
        try:
            nums.append(alphabet.index(letter))
        except ValueError:
            pass

    return nums


def to_letters(nums):
    return ''.join(alphabet[n] for n in nums)


class Enigma:
    def __init__(self, num_wheels=5):
        assert(num_wheels > 0)
        self.wheels = [Wheel(len(alphabet)) for i in range(num_wheels)]
        self.reflector = Wheel(len(alphabet))

    def run_through_wheels(self, number):
        for w in self.wheels:
            number = w.transform(number, True)
        number = self.reflector.reflect(number)
        for w in reversed(self.wheels):
            number = w.transform(number, False)
        return number

    def advance_wheels(self):
        for w in self.wheels:
            wrapped_around = w.advance()
            if not wrapped_around:
                break

    def encrypt(self, input):
        output_numbers = []
        for number in to_numbers(input):
            self.advance_wheels()
            output_numbers.append(self.run_through_wheels(number))
        return to_letters(output_numbers)


if __name__ == "__main__":
    import math

    print("Our alphabet has {} characters; the output should thus have {} bits per byte".format(len(alphabet),
                                                                                                math.log2(len(alphabet))),
          file=sys.stderr)

    random.seed(0)
    e = Enigma()

    for line in sys.stdin:
        print(e.encrypt(line))
