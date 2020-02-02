"""
https://en.wikipedia.org/wiki/Jefferson_wheel
"""

import itertools
import random
import string
import sys


class Disk(list):
    def __init__(self):
        super().__init__(string.ascii_lowercase)
        random.shuffle(self)

    def turn_to_display(self, character):
        # Our caller will set character to None when it's finished a
        # string, but still has some more wheels to process, including
        # us.  We randomize ourselves to erase any previous setting,
        # which could be distracting.
        if character is None:
            character = random.choice(self)
        assert character in self, f"wtf is {character=}"
        while self[0] != character:
            self.append(self.pop(0))

    def __str__(self):
        return ''.join(self)


def chunk(iterable, size):
    for i in range(0, len(iterable), size):
        yield iterable[i:i + size]


class WheelCipher():
    def __init__(self, num_disks=36):
        self.disks = [Disk() for _ in range(num_disks)]

    @property
    def width(self):
        return len(self.disks)

    @property
    def alphabet(self):
        return self.disks[0]

    @property
    def circumference(self):
        return len(self.alphabet)

    def turn_to_display(self, str_):
        str_ = [l for l in str_ if l in self.alphabet]
        for disk, character in itertools.zip_longest(self.disks, str_):
            disk.turn_to_display(character)

    def random_row(self):
        offset = random.randrange(1, self.circumference)
        for d in self.disks:
            yield d[offset]

    def _filter_string(self, str_):
        return [c for c in str_.lower() if c in self.alphabet]

    def encrypt_string(self, str_):
        str_ = self._filter_string(str_)
        result = []
        for c in chunk(str_, self.width):
            self.turn_to_display(c)
            result.extend(self.random_row())
        return ''.join(result)

    def decrypt_string(self, str_):
        str_ = self._filter_string(str_)
        for index, c in enumerate(chunk(str_, self.width)):
            self.turn_to_display(c)
            if index > 0:
                print()
            print(self)

    def __str__(self):
        result = []
        for offset in range(self.circumference):
            row = []
            for d in self.disks:
                row.append(d[offset])
            result.append(''.join(row))
        return '\n'.join(result)


if __name__ == "__main__":
    random.seed(0)
    j = WheelCipher()
    plaintext = ''.join(sys.argv[1:])
    if not plaintext:
        plaintext = "Attack at dawn"
    print(f"{plaintext=}")
    ciphertext = ' '.join(chunk(j.encrypt_string(plaintext), 5))
    print(f"{ciphertext=}")
    j.decrypt_string(ciphertext)
