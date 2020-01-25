import string
import itertools


the_alphabet = itertools.cycle(string.ascii_letters)


def number_to_letter(number):
    return next(itertools.islice(itertools.cycle(string.ascii_letters), number, number + 1))


def letter_to_number(l):
    return string.ascii_letters.index(l)


def rotate_letter(character, amount):
    try:
        n = letter_to_number(character)
    except ValueError:
        return character

    return number_to_letter(n + amount)


def caesar(text, encrypt=True):
    offset = 3 if encrypt else -3
    return ''.join([rotate_letter(l, offset) for l in text])


if __name__ == "__main__":
    cleartext = "When in Rome, do as the Romans do"
    ciphertext = caesar(cleartext)
    print(ciphertext)
    print(caesar(ciphertext, encrypt=False))
