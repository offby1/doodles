import string
import itertools


lets = itertools.cycle(string.ascii_letters)
offset = itertools.dropwhile(lambda c: c < 'd', itertools.cycle(string.ascii_letters))

forward_mapping = dict(itertools.islice(zip(lets, offset), len(string.ascii_letters)))
reverse_mapping = dict(itertools.islice(zip(offset, lets), len(string.ascii_letters)))


def caesar(text, encrypt=True):
    map_ = forward_mapping if encrypt else reverse_mapping
    return ''.join([map_.get(_, _) for _ in text])


if __name__ == "__main__":
    cleartext = "When in Rome, do as the Romans do"
    ciphertext = caesar(cleartext)
    print(ciphertext)
    print(caesar(ciphertext, encrypt=False))
