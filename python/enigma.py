#!/usr/bin/env python3
import itertools
import random
import sys
from types import Iterator

""" This emulates the famous Enigma
(https://en.wikipedia.org/wiki/Enigma_machine) machine.  Encryption
and decryption are the same operation.  Example usage:

    $ echo fee fi fo fum | python3 enigma.py | tee >(base64 -i - > /dev/tty) | python3 enigma.py
    1tWxuC6SrLfeyOkynyk=
    fee fi fo fum

"""


class Rotor:
    def __init__(self, num_slots: int) -> None:

        # Otherwise reflection won't work ... I think
        assert (num_slots % 2 == 0)

        self.offset = 0
        self.num_slots = num_slots
        self.permutation = Permutation(self.num_slots)

    def advance(self):
        self.offset += 1

        if self.offset == self.num_slots:
            self.offset = 0

        # The return value is True if we've "wrapped around".
        return self.offset == 0

    def transform(self, number: int, encrypt: bool) -> int:
        method_name = 'permute' if encrypt else 'unpermute'
        return getattr(self.permutation, method_name)(number, self.offset)


def _invert_list(numbers: List[int]) -> List[int]:
    inverse = numbers.copy()
    for index, value in enumerate(numbers):
        inverse[value] = index
    return inverse


def _invert_tableau(tableau: List[List[int]]) -> List[List[int]]:
    return [_invert_list(l) for l in tableau]


def _make_tableau(numbers: List[int]) -> Iterator[List[int]]:
    l = len(numbers)
    cycle = itertools.cycle(numbers)

    for i in range(l):
        yield list(itertools.islice(cycle, l))
        next(cycle)


class Permutation:
    def __init__(self, length: int) -> None:
        numbers = list(range(length))
        random.shuffle(numbers)

        self.forward_tableau = list(_make_tableau(numbers))
        self.reverse_tableau = _invert_tableau(self.forward_tableau)

    def permute(self, input: int, offset: int) -> int:
        return self.forward_tableau[offset][input]

    def unpermute(self, input: int, offset: int) -> int:
        return self.reverse_tableau[offset][input]


class Enigma:
    def __init__(self, num_rotors: int =5) -> None:
        assert(num_rotors > 0)
        self.rotors = [Rotor(256) for i in range(num_rotors)]

    def reflect(self, number: int) -> int:
        n = self.rotors[0].num_slots
        offset = n // 2
        return (number + offset) % n

    def run_through_rotors(self, number: int) -> int:
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

    def encrypt(self, input_bytes: bytes) -> Iterator[int]:
        for number in input_bytes:
            yield self.run_through_rotors(number)
            self.advance_rotors()


if __name__ == "__main__":
    random.seed(0)
    e = Enigma()

    for line in sys.stdin.buffer:
        sys.stdout.buffer.write(bytes(e.encrypt(line)))
