#!/usr/bin/env python3
import itertools
import pprint
import random
import string
import sys
from typing import Any, Dict, Iterator, List, Optional

import click  # pip install click

# This emulates the famous [Enigma machine]
# (https://en.wikipedia.org/wiki/Enigma_machine).  Encryption
# and decryption are the same operation.  Example usage:

#     $ echo fee fi fo fum | python3 enigma.py frotz | tee >(base64 -i - > /dev/tty) | python3 enigma.py frotz
#     YWp5anFucnlkaHF2cg==
#     fee fi fo fum

# Check the types by doing
#       $ python3 -m pip install --user mypy-lang
#       $ python3 -m mypy --strict enigma.py


class Rotor:
    ALPHABET = list(string.ascii_lowercase + ' .')

    """
    >>> random.seed(0)
    >>> p = Rotor(4)
    >>> p.transform(0, True)
    2
    >>> p.transform(2, False)
    0
    """

    def __init__(self) -> None:
        # Otherwise reflection won't work
        assert len(self.ALPHABET) % 2 == 0

        self.offset = 0

        self.alphabet = itertools.cycle(self.ALPHABET)
        self.shuffled: Any = self.ALPHABET[:]
        random.shuffle(self.shuffled)
        self.shuffled = itertools.cycle(self.shuffled)

        self.compute_mappings()

    def compute_mappings(self) -> None:
        alphabet = itertools.islice(self.alphabet, self.num_slots)
        shuffled = itertools.islice(self.shuffled, self.num_slots)
        self.forward_mapping: Dict[str, str] = {a: s for a, s in zip(alphabet, shuffled)}
        alphabet = itertools.islice(self.alphabet, self.num_slots)
        shuffled = itertools.islice(self.shuffled, self.num_slots)
        self.reverse_mapping: Dict[str, str] = {s: a for a, s in zip(alphabet, shuffled)}

    @property
    def num_slots(self) -> int:
        return len(self.ALPHABET)

    def advance(self) -> bool:  # True means we "wrapped around"
        next(self.alphabet)
        self.compute_mappings()

        self.offset += 1

        if self.offset == self.num_slots:
            self.offset = 0
            return True

        return False

    def transform(self, letter: str, encrypt: bool) -> str:
        mapping_ = self.forward_mapping if encrypt else self.reverse_mapping
        return mapping_[letter]

    def __repr__(self) -> str:
        return pprint.pformat(self.forward_mapping)


class Enigma:
    def __init__(self, num_rotors: int = 5) -> None:
        assert num_rotors > 0
        self.rotors = [Rotor() for _ in range(num_rotors)]

    @property
    def rotor_size(self) -> int:
        return self.rotors[0].num_slots

    @property
    def alphabet(self) -> List[str]:
        return self.rotors[0].ALPHABET

    def reflect(self, letter: str) -> str:
        index = self.alphabet.index(letter)
        offset = self.rotor_size // 2
        return self.alphabet[(index + offset) % self.rotor_size]

    def run_through_rotors(self, letter: str) -> str:
        for r in self.rotors:
            letter = r.transform(letter, True)
        letter = self.reflect(letter)
        for r in reversed(self.rotors):
            letter = r.transform(letter, False)
        return letter

    def advance_rotors(self) -> None:
        for r in self.rotors:
            wrapped_around: bool = r.advance()
            if not wrapped_around:
                break

    def encrypt_single_letter(self, letter: str) -> Optional[str]:
        return_value = None

        if letter in self.alphabet:
            return_value = self.run_through_rotors(letter)
            self.advance_rotors()

        return return_value

    def encrypt(self, input_letters: str) -> Iterator[str]:
        input_letters = input_letters.lower()

        for letter in input_letters:
            encrypted = self.encrypt_single_letter(letter)
            if encrypted is not None:
                yield encrypted


@click.command()
@click.argument("secret_key", default="")
def encrypt_stdin_to_stdout(secret_key: str) -> None:
    random.seed(secret_key)
    e = Enigma(num_rotors=5)

    for line in sys.stdin:
        wat = e.encrypt(line)
        sys.stdout.write(''.join(wat))


if __name__ == "__main__":
    r = Rotor()
    encrypt_stdin_to_stdout()
