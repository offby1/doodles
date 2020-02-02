import itertools
import random
import string

lets = string.printable

scrambled = list(lets)
random.shuffle(scrambled)
scrambled = ''.join(scrambled)

forward_mapping = dict(zip(lets, scrambled))
reverse_mapping = dict(zip(scrambled, lets))

def transform(letter, encrypt=True):
    map_ = forward_mapping if encrypt else reverse_mapping
    return map_.get(letter, letter)


with open ('/etc/passwd') as inf:
    more_sample_text = inf.read()
    with open('/tmp/decrypt-me', 'w') as outf:
        outf.write(''.join(transform(l) for l in more_sample_text))
        print(f'Check out {outf.name!r} for some ciphertext')

    with open('/tmp/decrypt-me', 'r') as inf:
        print(''.join(transform(l, encrypt=False) for l in inf.read()))
