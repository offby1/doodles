"""Given a string of letters, and an English dictionary separated into
words, return another string that is like the first, but with spaces
added, so that the result is a space-separated sequence of words.

For example, given "thiswasatrickyproblemitdoescertainlyappear", and a
normal English dictionary, return "this was a tricky problem it does
certainly appear".

"""

# python3 -m pytest favorite-interview-question-insert-spaces.py


def _snarf_dict():
    result = set()
    with open('/usr/share/dict/words') as inf:
        for word in inf:
            word = word.rstrip().lower()

            # Eliminate most of the one-letter words from the
            # dictionary, because they make the problem a lot less
            # interesting.

            if len(word) == 1:
                if word.lower() not in {'i', 'a'}:
                    continue

            # Similarly only keep words if they contain a vowel.
            if not any(v in word for v in {'a', 'e', 'i', 'o', 'u', 'y'}):
                print(f"Vowel-less monstrosity, begone {word!r}")
                continue

            result.add(word)
    return result


dictionary_words = _snarf_dict()


def _all_splits(seq):
    for index in range(1, len(seq)):
        yield (seq[0:index], seq[index:])


def insert_spaces(input_, dictionary_words):
    print(f'input_ {input_!r}')
    if input_ == '':
        return ''

    if input_ in dictionary_words:
        return input_

    for prefix, rest in _all_splits(input_):
        if prefix in dictionary_words:
            print(f'Hey, {input_!r} starts with {prefix!r}')
            from_shorter_string = insert_spaces(rest, dictionary_words)
            if from_shorter_string:
                rv = prefix + ' ' + from_shorter_string
                print(f'w00t! {rv}')
                return rv
            print(f'hmm, well I guess {rest!r} is insoluble')

    return None


def test_base_case():
    assert insert_spaces('', dictionary_words) == ''


def test_lone_word():
    assert insert_spaces("beer", dictionary_words) == "beer"


def test_two_words():
    assert insert_spaces("dickmove", dictionary_words) == "dick move"


def test_example():
    # unfortunately the output depends heavily on the dicitonary.
    # This works on the dict that comes with macOS 10.13.3 "Sierra".
    input_ = "thiswasatrickyproblemitdoescertainlyappear"
    expected_output = "this wa sa tricky problem it do es certain ly appear"

    assert insert_spaces(input_, dictionary_words) == expected_output
