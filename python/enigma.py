#!/usr/bin/env python3
import random
import string

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

    def run_through_wheels(self, letter, encrypt):
        for w in self.wheels if encrypt else reversed(self.wheels):
            letter = w.transform(letter, encrypt)
        return letter

    def advance_wheels(self):
        for w in self.wheels:
            wrapped_around = w.advance()
            if not wrapped_around:
                break

    def process(self, input, encrypt):
        output_numbers = []
        for number in to_numbers(input):
            self.advance_wheels()
            output_numbers.append(self.run_through_wheels(number, encrypt))
        return to_letters(output_numbers)

    def encrypt(self, input):
        return self.process(input, True)

    def decrypt(self, input):
        return self.process(input, False)

if __name__ == "__main__":
    import sys

    lines = sys.stdin.readlines()

    for method in 'encrypt', 'decrypt':
        random.seed(0)
        e = Enigma()

        for line in lines:
            print(getattr(e, method)(line))
