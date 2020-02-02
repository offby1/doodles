import string
import itertools


lets = itertools.cycle(string.ascii_letters)
offset = itertools.dropwhile(lambda c: c < 'd', itertools.cycle(string.ascii_letters))

forward_mapping = dict(itertools.islice(zip(lets, offset), 52))
reverse_mapping = dict(itertools.islice(zip(offset, lets), 52))


def caesar(text, encrypt=True):
    map_ = forward_mapping if encrypt else reverse_mapping
    return ''.join([map_.get(l, l) for l in text])


if __name__ == "__main__":
    cleartext = "When in Rome, do as the Romans do"
    ciphertext = caesar(cleartext)
    print(ciphertext)
    print(caesar(ciphertext, encrypt=False))
