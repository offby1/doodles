def n_letter_words(dict_file_name, n):
    with open(dict_file_name) as inf:
        for line in inf:
            line = line.lower().rstrip()
            if len(line) == n:
                yield (line)


def main():
    import pprint
    pprint.pprint(set(n_letter_words('/usr/share/dict/words', 5)))

if __name__ == "__main__":
    main()
