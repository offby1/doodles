#!/usr/bin/env python3
import itertools
import random
import sys
from typing import Iterator, List

import click                    # pip install click

""" This emulates the famous Enigma
(https://en.wikipedia.org/wiki/Enigma_machine) machine.  Encryption
and decryption are the same operation.  Example usage:

    $ echo fee fi fo fum | python3 enigma.py frotz | tee >(base64 -i - > /dev/tty) | python3 enigma.py frotz
    TUxq0hlfHKU4zzOmmrY=
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
    length = len(numbers)
    cycle = itertools.cycle(numbers)

    for i in range(length):
        yield list(itertools.islice(cycle, length))
        next(cycle)


class Permutation:
    def __init__(self, length: int) -> None:
        numbers = list(range(length))
        random.shuffle(numbers)

        self.forward_tableau = list(_make_tableau(numbers))
        self.reverse_tableau = _invert_tableau(self.forward_tableau)

    def permute(self, inp: int, offset: int) -> int:
        return self.forward_tableau[offset][inp]

    def unpermute(self, inp: int, offset: int) -> int:
        return self.reverse_tableau[offset][inp]


class Enigma:
    def __init__(self, num_rotors: int =5) -> None:
        assert(num_rotors > 0)
        self.num_slots = 256
        self.rotors = [Rotor(self.num_slots) for i in range(num_rotors)]

    def reflect(self, number: int) -> int:
        offset = self.num_slots // 2
        return (number + offset) % self.num_slots

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


@click.command()
@click.argument('secret_key', default='')
def encrypt_stdin_to_stdout(secret_key):
    random.seed(secret_key)
    e = Enigma()

    for line in sys.stdin.buffer:
        sys.stdout.buffer.write(bytes(e.encrypt(line)))


if __name__ == "__main__":
    encrypt_stdin_to_stdout()
