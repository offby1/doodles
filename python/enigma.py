#!/usr/bin/env python3
import random
import sys
from typing import Iterator, List

import click  # pip install click

""" This emulates the famous Enigma
(https://en.wikipedia.org/wiki/Enigma_machine) machine.  Encryption
and decryption are the same operation.  Example usage:

    $ echo fee fi fo fum | python3 enigma.py frotz | tee >(base64 -i - > /dev/tty) | python3 enigma.py frotz
    TUxq0hlfHKU4zzOmmrY=
    fee fi fo fum
"""

"""
Check the types by doing
      $ python3 -m pip install --user mypy-lang
      $ python3 -m mypy enigma.py
"""


class Rotor:
    """
    >>> random.seed(0)
    >>> p = Rotor(4)
    >>> p.transform(0, True)
    2
    >>> p.transform(2, False)
    0
    """

    def __init__(self, num_slots: int) -> None:

        # Otherwise reflection won't work ... I think
        assert num_slots % 2 == 0

        self.offset = 0
        self.num_slots = num_slots

        self.forward_numbers = list(range(self.num_slots))
        random.shuffle(self.forward_numbers)
        self.reverse_numbers = _invert_list(self.forward_numbers)

    def advance(self):
        self.forward_numbers.append(self.forward_numbers.pop(0)) # rotate the list
        self.reverse_numbers = _invert_list(self.forward_numbers)

        self.offset += 1

        if self.offset == self.num_slots:
            self.offset = 0

        # The return value is True if we've "wrapped around".
        return self.offset == 0

    def transform(self, number: int, encrypt: bool) -> int:
        list_ = self.forward_numbers if encrypt else self.reverse_numbers
        return list_[number]


def _invert_list(numbers: List[int]) -> List[int]:
    inverse = numbers.copy()
    for index, value in enumerate(numbers):
        inverse[value] = index
    return inverse


class Enigma:
    def __init__(self, num_rotors: int = 5) -> None:
        assert num_rotors > 0
        self.num_slots = 256
        self.rotors = [Rotor(self.num_slots) for _ in range(num_rotors)]

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
@click.argument("secret_key", default="")
def encrypt_stdin_to_stdout(secret_key):
    random.seed(secret_key)
    e = Enigma()

    for line in sys.stdin.buffer:
        sys.stdout.buffer.write(bytes(e.encrypt(line)))


if __name__ == "__main__":
    encrypt_stdin_to_stdout()
