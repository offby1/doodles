def run_length_encode(inp):
    current_char = None
    current_count = None
    for ch in inp:
        if ch != current_char:
            if current_char is not None:
                yield (current_char, current_count)
            current_char = ch
            current_count = 1

        else:
            current_count += 1
    yield (current_char, current_count)


def tidy_up_rle(pairs):
    result_chars = []
    for p in pairs:
        result_chars.append(f"{p[0]}{p[1]}")
    return ''.join(result_chars)

# tidy_up_rle(run_length_encode('xyxx!hello world'))
# 'x1y1x2!1h1e1l2o1 1w1o1r1l1d1'
# >>>
