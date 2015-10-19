#!/usr/bin/env python3
import random
import string
import sys

alphabet = string.ascii_letters + string.digits + string.punctuation


def rotate(l):
    l.append (l.pop (0))


class Rotor:
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


def to_string(nums):
    return ''.join(alphabet[n] for n in nums)


class Enigma:
    def __init__(self, num_rotors=5):
        assert(num_rotors > 0)
        self.rotors = [Rotor(len(alphabet)) for i in range(num_rotors)]
        self.reflector = Rotor(len(alphabet))

    def run_through_rotors(self, number):
        for r in self.rotors:
            number = r.transform(number, True)
        number = self.reflector.reflect(number)
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
        print(e.encrypt(line))
