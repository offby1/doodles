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
        self.offset += 1

        # The return value is True if we've "wrapped around".
        if self.offset == self.num_slots:
            self.offset = 0
            return True
        return False

    def transform(self, number, encrypt):
        return self.permutation.permute(number, self.offset) if encrypt else self.permutation.unpermute(number, self.offset)


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

    def permute(self, input, offset):
        return self.numbers[(input + offset) % len(self.numbers)]

    def unpermute(self, input, offset):
        return (self.numbers.index(input) - offset) % len(self.numbers)


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
    random.seed(0)
    e = Enigma()

    for line in sys.stdin:
        print(e.encrypt(line), end='')
