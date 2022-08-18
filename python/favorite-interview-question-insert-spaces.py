"""Given a string of letters, and an English dictionary separated into
words, return another string that is like the first, but with spaces
added, so that the result is a space-separated sequence of words.

For example, given "thisisatrickyproblemitcertainlydoesappear", and a
normal English dictionary, return "this is a tricky problem it certainly
does appear".

"""

# python3 -m pytest --doctest-modules  favorite-interview-question-insert-spaces.py


def bogus_two_letter_word(w):
    if len(w) != 2:
        return False

    # It's easier to spell out the legit words than the bogus ones
    return w not in {
        "am",
        "an",
        "as",
        "at",
        "ax",
        "be",
        "by",
        "do",
        "ha",
        "he",
        "hi",
        "ho",
        "id",
        "if",
        "in",
        "is",
        "it",
        "me",
        "my",
        "no",
        "of",
        "ok",
        "on",
        "or",
        "so",
        "to",
        "us",
    }


def _snarf_dict():
    result = set()
    with open("/usr/share/dict/words") as inf:
        for raw_word in inf:
            word = raw_word.rstrip().lower()

            # Eliminate most of the one-letter words from the
            # dictionary, because they make the problem a lot less
            # interesting, and the algorithm much slower.
            if len(word) == 1:
                if word.lower() not in {"i", "a"}:
                    continue

            if bogus_two_letter_word(word):
                continue

            result.add(word)
    return result


dictionary_words = _snarf_dict()


def _all_splits(seq):
    """
    >>> list(_all_splits('abcd'))
    [('a', 'bcd'), ('ab', 'cd'), ('abc', 'd')]
    """
    for index in range(1, len(seq)):
        yield (seq[0:index], seq[index:])


def insert_spaces(input_, dictionary_words):
    if input_ in dictionary_words:
        yield input_

    for prefix, rest in _all_splits(input_):
        if prefix in dictionary_words:
            from_shorter_string = insert_spaces(rest, dictionary_words)
            for short in from_shorter_string:
                result = prefix + " " + short
                yield result


def test_base_case():
    assert list(insert_spaces("", dictionary_words)) == []


def test_lone_word():
    assert "beer" in insert_spaces("beer", dictionary_words)


def test_two_words():
    assert "dick move" in insert_spaces("dickmove", dictionary_words)


def test_hey_I_should_really_come_up_with():
    assert "I should come up with a case that will foil a simpleminded non-recursive solution" is True


def test_example():
    # unfortunately the output depends heavily on the dictionary.
    # This works on the dict that comes with macOS 10.13.3 "Sierra".
    input_ = "thisisatrickyproblemitcertainlydoesappear"
    expected_output = "this is a tricky problem it certainly does appear"

    assert expected_output in insert_spaces(input_, dictionary_words)


if __name__ == "__main__":
    import re

    def num_spaces(str_):
        return (len(str_.split()), str_)

    for inp_ in (
        "thisisatrickyproblemitcertainlydoesappear",
        "tell me your dream, your hope, your fear",
        "I suppose pretty much anything you put in here would have a number of solution",
    ):
        cleaned_up_input = re.sub(r"[^a-z]", "", inp_.lower())

        solutions = sorted(
            insert_spaces(cleaned_up_input, dictionary_words), key=num_spaces
        )
        print("\n".join(solutions))
