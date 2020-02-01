import itertools
import secrets

RANDOM_ARRAY = [secrets.randbelow(256) for _ in range(64)]


def text_to_numbers(text):
    return list(text.encode('utf-8'))


def numbers_to_text(nums):
    return bytes(nums).decode('utf-8')


def _encrypt_number(plaintext, key):
    return (plaintext + key) % 256


def _decrypt_number(ciphertext, key):
    return (ciphertext - key) % 256


def mangle_numbers(nums, encrypt=True):
    func = _encrypt_number if encrypt else _decrypt_number
    for plaintext_number, key_number in zip(nums, itertools.cycle(RANDOM_ARRAY)):
        yield func(plaintext_number, key_number)


with open ('/Users/erichanchrow/git-repositories/3rd-party/emacs/nextstep/Emacs.app/Contents/Resources/etc/HELLO') as inf:
    more_sample_text = inf.read()
    with open('/tmp/decrypt-me', 'w') as outf:
        outf.buffer.write(bytes(mangle_numbers(text_to_numbers(more_sample_text))))
        print(f'Check out {outf.name!r} for some ciphertext')

    with open('/tmp/decrypt-me', 'rb') as inf:
        print(numbers_to_text(mangle_numbers(inf.read(), encrypt=False)))
