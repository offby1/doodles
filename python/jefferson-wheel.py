"""
https://en.wikipedia.org/wiki/Jefferson_wheel

Basically an Enigma where the wheels are fixed.
"""

import itertools
import random
import string


def l_to_n(letter):
    letter = letter.lower()
    assert letter in string.ascii_lowercase
    return ord(letter) - ord(string.ascii_lowercase[0])


def n_to_l(n):
    return string.ascii_lowercase[n]


def _invert_list(numbers):
    inverse = numbers.copy()
    for index, value in enumerate(numbers):
        inverse[value] = index
    return inverse


class Disk():
    def __init__(self, id_):
        self.id = id_
        self.encryption_letters = [l_to_n(l) for l in string.ascii_lowercase]
        random.shuffle(self.encryption_letters)
        self.decryption_letters = _invert_list(self.encryption_letters)

    def crypt(self, n, encrypt=True):
        if encrypt:
            rv = self.encryption_letters[n]
        else:
            rv = self.decryption_letters[n]

        return rv

    def __str__(self):
        return (
            f'{self.id:02} ' +
            ' '.join([n_to_l(n) for n in self.encryption_letters]))


class WheelCipher():
    def __init__(self, num_disks=36):
        self.disks = [Disk(n) for n in range(num_disks)]

    def crypt_number_sequence(self, numbers, encrypt=True):
        for number, disk in zip(numbers, itertools.cycle(self.disks)):
            yield disk.crypt(number, encrypt=encrypt)

    def crypt_string(self, str_, encrypt=True):
        str_ = [c for c in str_.lower() if c in string.ascii_lowercase]

        return ''.join([n_to_l(n)
                        for n in self.crypt_number_sequence([l_to_n(l) for l in str_],
                                                            encrypt=encrypt)])

    def __str__(self):
        return '\n'.join(str(d) for d in self.disks)


if __name__ == "__main__":
    random.seed(0)
    j = WheelCipher()
    print(j)
    wat = j.crypt_string('Hey you!!' * 20, encrypt=True)
    print(wat)
    print(j.crypt_string(wat, encrypt=False))
