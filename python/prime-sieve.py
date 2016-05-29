def integers():
    i = 2
    while True:
        yield i
        i += 1


def filter_sequence(input, predicate):
    for item in input:
        if predicate(item):
            yield item


def is_multiple_of(x, factor):
    return (x % factor) == 0


seq = filter_sequence(integers(), lambda x: not (is_multiple_of(x, 2)))
while True:
    o = next(seq)
    print(o)

    if o > 2000:
        break

    seq = filter_sequence(iter(seq), lambda x: not (is_multiple_of(x, o)))
