#!/usr/bin/env python3
import itertools
import random
import string
import sys

alphabet = string.printable

inverted_alphabet = {letter: number for number, letter in enumerate(alphabet)}


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
            nums.append(inverted_alphabet[letter])
        except KeyError:
            pass

    return nums


def to_string(nums):
    return ''.join(alphabet[n] for n in nums)


def _invert_list(numbers):
    inverse = numbers.copy()
    for index, value in enumerate(numbers):
        inverse[value] = index
    return inverse


def _invert_tableau(tableau):
    return [_invert_list(l) for l in tableau]


class Permutation:
    def __init__(self, length):
        numbers = list(range(length))
        random.shuffle(numbers)

        self.forward_tableau = list(self._make_tableau(numbers))
        self.reverse_tableau = _invert_tableau(self.forward_tableau)

    def permute(self, input, offset):
        return self.forward_tableau[offset][input]

    def unpermute(self, input, offset):
        return self.reverse_tableau[offset][input]

    def _make_tableau(self, numbers):
        l = len(numbers)
        cycle = itertools.cycle(numbers)

        for i in range(l):
            yield list(itertools.islice(cycle, l))
            next(cycle)


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