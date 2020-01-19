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

cleartext = "When in Rome, do as the Romans do"
ciphertext = ''.join([rotate_letter(l, 3) for l in cleartext])
print(ciphertext)
print(''.join([rotate_letter(l, -3) for l in ciphertext]))
