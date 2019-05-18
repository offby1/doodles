"""Given a string of letters, and an English dictionary separated into
words, return another string that is like the first, but with spaces
added, so that the result is a space-separated sequence of words.

For example, given "thisisatrickyproblemitcertainlydoesappear", and a
normal English dictionary, return "this was a tricky problem it does
certainly appear".

"""

# python3 -m pytest --doctest-modules  favorite-interview-question-insert-spaces.py

import os

import tqdm  # pip install tqdm


def _snarf_dict():
    result = set()
    with open("/usr/share/dict/words") as inf:
        progressbar = tqdm.tqdm(
            total=os.fstat(inf.fileno()).st_size, desc="Reading dictionary"
        )
        for raw_word in inf:
            progressbar.update(len(raw_word))
            word = raw_word.rstrip().lower()

            # Eliminate most of the one-letter words from the
            # dictionary, because they make the problem a lot less
            # interesting, and the algorithm much slower.
            if len(word) == 1:
                if word.lower() not in {"i", "a"}:
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


def insert_spaces(input_, dictionary_words, recursion_depth=0, progressbar=None):
    if progressbar is None:
        # total here is a total guess :-) It's here just to make the
        # progress bar render solid glyphs.
        progressbar = tqdm.tqdm(desc="Finding solutions", total=len(input_) * 4)

    if input_ in dictionary_words:
        if recursion_depth == 0:
            progressbar.write(input_)
        yield input_

    for prefix, rest in _all_splits(input_):
        if prefix in dictionary_words:
            from_shorter_string = insert_spaces(
                rest,
                dictionary_words,
                recursion_depth=recursion_depth + 1,
                progressbar=progressbar,
            )
            for short in from_shorter_string:
                result = prefix + " " + short
                if recursion_depth == 0:
                    progressbar.write(result)
                yield result
                progressbar.update()

    if recursion_depth == 0:
        progressbar.close()


def test_base_case():
    assert list(insert_spaces("", dictionary_words)) == []


def test_lone_word():
    assert "beer" in insert_spaces("beer", dictionary_words)


def test_two_words():
    assert "dick move" in insert_spaces("dickmove", dictionary_words)


def test_example():
    # unfortunately the output depends heavily on the dicitonary.
    # This works on the dict that comes with macOS 10.13.3 "Sierra".
    input_ = "thisisatrickyproblemitcertainlydoesappear"
    expected_output = "this is a tricky problem it certainly does appear"

    assert expected_output in insert_spaces(input_, dictionary_words)


if __name__ == "__main__":
    import re
    import statistics

    def _average_word_length(str_):
        words = str_.split(" ")
        return statistics.mean([len(w) for w in words])

    for inp_ in (
        "thisisatrickyproblemitcertainlydoesappear",
        "tell me your dream, your hope, your fear",
        "I suppose pretty much anything you put in here would have a number of solution",
    ):
        cleaned_up_input = re.sub(r"[^a-z]", "", inp_.lower())

        for index, _ in enumerate(insert_spaces(cleaned_up_input, dictionary_words)):
            if index == 10:
                break
