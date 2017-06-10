import collections
import string


ALPHABET = string.printable


def slightly_permute_from_elements(seq, elements):
    # Eliminate dups while preserving order
    result = list(collections.OrderedDict.fromkeys (elements))

    for elt in seq:
        if elt not in result:
            result.append(elt)

    return result


def test_base_case():
    original_seq = list(range(10))
    new_seq = slightly_permute_from_elements(original_seq, [])
    assert new_seq == original_seq


def test_preserves_elements():
    original_seq = set(range(20))
    new_seq = slightly_permute_from_elements(original_seq, range(10))
    assert set(new_seq) == original_seq


def slightly_permuted_alphabet(key_string):
    return slightly_permute_from_elements(ALPHABET, key_string)


def make_alphabet_mapping(letters):
    return dict(zip(ALPHABET, letters))


def invert_mapping(m):
    return {v: k for k, v in m.items()}


def encrypt_message_with_mapping(message, mapping):
    result = []
    for letter in message:
        encrypted = mapping.get(letter)
        if encrypted is not None:
            result.append(encrypted)

    return ''.join(result)


def encrypt_message_from_key(message, key):
    alf = slightly_permuted_alphabet(key)
    mapping = make_alphabet_mapping(alf)
    return encrypt_message_with_mapping(message, mapping)


def decrypt_message_from_key(message, key):
    alf = slightly_permuted_alphabet(key)
    mapping = invert_mapping(make_alphabet_mapping(alf))
    return encrypt_message_with_mapping(message, mapping)


def test_round_trip():
    plaintext = "hey everyone! Let's have cheeseburgers for breakfast!!"
    ciphertext = encrypt_message_from_key (plaintext, 'frotz')
    recovered = decrypt_message_from_key (ciphertext, 'frotz')
    assert recovered == plaintext
