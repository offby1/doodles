#!/usr/bin/env python3
import random
import sys
from typing import Iterator, List, Optional

import click  # pip install click

# This emulates the famous Enigma
# (https://en.wikipedia.org/wiki/Enigma_machine) machine.  Encryption
# and decryption are the same operation.  Example usage:

#     $ echo fee fi fo fum | python3 enigma.py frotz | tee >(base64 -i - > /dev/tty) | python3 enigma.py frotz
#     cnFxamVldmJudA==
#     feefifofum

# Check the types by doing
#       $ python3 -m pip install --user mypy-lang
#       $ python3 -m mypy enigma.py


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

        self.forward_numbers = list(range(num_slots))
        random.shuffle(self.forward_numbers)
        self.reverse_numbers = _invert_list(self.forward_numbers)

    @property
    def num_slots(self) -> int:
        return len(self.forward_numbers)

    def advance(self) -> bool:  # True means we "wrapped around"
        self.forward_numbers.append(self.forward_numbers.pop(0))  # rotate the list
        self.reverse_numbers = _invert_list(self.forward_numbers)

        self.offset += 1

        if self.offset == self.num_slots:
            self.offset = 0
            return True

        return False

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
        self.rotors = [Rotor(26) for _ in range(num_rotors)]

    @property
    def rotor_size(self) -> int:
        return self.rotors[0].num_slots

    def reflect(self, number: int) -> int:
        offset = self.rotor_size // 2
        return (number + offset) % self.rotor_size

    def run_through_rotors(self, number: int) -> int:
        for r in self.rotors:
            number = r.transform(number, True)
        number = self.reflect(number)
        for r in reversed(self.rotors):
            number = r.transform(number, False)
        return number

    def advance_rotors(self) -> None:
        for r in self.rotors:
            wrapped_around: bool = r.advance()
            if not wrapped_around:
                break

    def encrypt_single_number(self, number: int) -> Optional[int]:
        return_value = None

        number -= ord('a')
        if 0 <= number < self.rotor_size:
            return_value = self.run_through_rotors(number) + ord('a')
            self.advance_rotors()

        return return_value

    def encrypt(self, input_bytes: bytes) -> Iterator[int]:
        input_bytes = bytes([b for b in input_bytes.lower()])
        input_bytes = input_bytes.lower()

        for number in input_bytes:
            encrypted = self.encrypt_single_number(number)
            if encrypted is not None:
                yield encrypted


@click.command()
@click.argument("secret_key", default="")
def encrypt_stdin_to_stdout(secret_key):
    random.seed(secret_key)
    e = Enigma()

    for line in sys.stdin.buffer:
        sys.stdout.buffer.write(bytes(e.encrypt(line)))


if __name__ == "__main__":
    encrypt_stdin_to_stdout()
