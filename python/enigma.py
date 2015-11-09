#!/usr/bin/env python3
import random
import string
import sys

alphabet = string.printable


class Rotor:
    def __init__(self, num_slots):
        self.offset = 0
        self.num_slots = num_slots
        self.permutation = Permutation(self.num_slots)

    def advance(self):
        # Rotate the list.
        self.permutation.numbers.append(self.permutation.numbers.pop(0))

        self.offset += 1

        if self.offset == self.num_slots:
            self.offset = 0
            return True
        return False

    def transform(self, number, encrypt):
        return self.permutation.permute(number) if encrypt else self.permutation.unpermute(number)


def to_numbers(str):
    nums = []
    for letter in str:
        try:
            nums.append(alphabet.index(letter))
        except ValueError:
            pass

    return nums


def to_string(nums):
    return ''.join(alphabet[n] for n in nums)


class Permutation:
    def __init__(self, length):
        self.numbers = list(range(length))
        random.shuffle(self.numbers)

    def permute(self, input):
        return self.numbers[input]

    def unpermute(self, input):
        return self.numbers.index(input)


class Enigma:
    def __init__(self, num_rotors=5):
        assert(num_rotors > 0)
        self.rotors = [Rotor(len(alphabet)) for i in range(num_rotors)]

    def reflect(self, number):
        n = self.rotors[0].num_slots
        offset = n // 2
        return (number + offset) % n

    def run_through_rotors(self, number):
        for r in self.rotors:
            number = r.transform(number, True)
        number = self.reflect(number)
        for r in reversed(self.rotors):
            number = r.transform(number, False)
        return number

    def advance_rotors(self):
        for r in self.rotors:
            wrapped_around = r.advance()
            if not wrapped_around:
                break

    def encrypt(self, input):
        output_numbers = []
        for number in to_numbers(input):
            self.advance_rotors()
            output_numbers.append(self.run_through_rotors(number))
        return to_string(output_numbers)


if __name__ == "__main__":
    import math

    print("Our alphabet has {} characters; the output should thus have {} bits per byte".format(len(alphabet),
                                                                                                math.log2(len(alphabet))),
          file=sys.stderr)

    random.seed(0)
    e = Enigma()

    for line in sys.stdin:
        print(e.encrypt(line), end='')
