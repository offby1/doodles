"""
https://en.wikipedia.org/wiki/Jefferson_wheel
"""

import itertools
import random
import string
import subprocess
import sys


class Disk(list):
    def __init__(self):
        super().__init__(string.ascii_lowercase)
        random.shuffle(self)

    def turn_to_display(self, character):
        # Our caller will set character to None when it's finished a
        # string, but still has some more disks to process, including
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

    def _turn_to_display(self, str_):
        for disk, character in itertools.zip_longest(self.disks, str_):
            disk.turn_to_display(character)

    def _random_row(self):
        offset = random.randrange(1, self.circumference)
        for d in self.disks:
            yield d[offset]

    def _filter_string(self, str_):
        return [c for c in str_.lower() if c in self.alphabet]

    def encrypt_string(self, str_):
        str_ = self._filter_string(str_)
        result = []
        for c in chunk(str_, self.width):
            self._turn_to_display(c)
            result.extend(self._random_row())
        return ''.join(result)

    def decrypt_string(self, str_):
        str_ = self._filter_string(str_)
        for index, c in enumerate(chunk(str_, self.width)):
            self._turn_to_display(c)

            if index > 0:
                yield('')

            # Sort the rows with the least "entropy" first.  This
            # makes it slightly easier to find the plaintext row from
            # amongst the garbage.
            for r in sorted(self.rows(), key=walker_entropy):
                yield(r)

    def rows(self):
        for offset in range(self.circumference):
            row = []
            for d in self.disks:
                row.append(d[offset])
            yield(''.join(row))

    def __str__(self):
        return '\n'.join(self.rows())


def walker_entropy(str_):
    """
    http://fourmilab.ch/random/
    $ brew install ent
    """

    child = subprocess.run(
        ["/usr/local/bin/ent"],
        input=str_,
        stdout=subprocess.PIPE,
        text=True
    )

    for line in child.stdout.split('\n'):
        # e.g. `Entropy = 4.283381 bits per byte.`
        fields = line.split(' ')
        if fields[0] == 'Entropy':
            return float(fields[2])


if __name__ == "__main__":
    random.seed(0)              # extra security :-)
    j = WheelCipher()
    plaintext = ''.join(sys.argv[1:])
    if not plaintext:
        plaintext = "Attack at dawn"
    print(f"{plaintext=}")
    ciphertext = ' '.join(chunk(j.encrypt_string(plaintext), 5))
    print(f"{ciphertext=}")
    print()
    for decrypted_row in j.decrypt_string(ciphertext):
        print(decrypted_row)
