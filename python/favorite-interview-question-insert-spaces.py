"""Given a string of letters, and an English dictionary separated into
words, return another string that is like the first, but with spaces
added, so that the result is a space-separated sequence of words.

For example, given "thisisatrickyproblemitcertainlydoesappear", and a
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
            # interesting, and the algorithm much slower.
            if len(word) == 1:
                if word.lower() not in {'i', 'a'}:
                    continue

            result.add(word)
    return result


dictionary_words = _snarf_dict()


def _all_splits(seq):
    for index in range(1, len(seq)):
        yield (seq[0:index], seq[index:])


    if input_ in dictionary_words:
        yield input_

    for prefix, rest in _all_splits(input_):
        if prefix in dictionary_words:
            from_shorter_string = insert_spaces(rest, dictionary_words)
            for short in from_shorter_string:
                yield prefix + ' ' + short

def test_base_case():
    assert list(insert_spaces('', dictionary_words)) == []


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
        words = str_.split(' ')
        return statistics.mean([len(w) for w in words])

    for inp_ in (
            "thisisatrickyproblemitcertainlydoesappear",
            "tell me your dream, your hope, your fear",
            "I suppose pretty much anything you put in here would have a number of solution"
    ):
        cleaned_up_input = re.sub (r'[^a-z]', '', inp_.lower())
        solutions = sorted(insert_spaces(cleaned_up_input, dictionary_words),
                           key=_average_word_length,
                           reverse=True)
        if solutions:
            print(f'\n\n{len(solutions)} solutions!')
            for solution in solutions[0:10]:
                print(f'{solution}')
