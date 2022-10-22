def next_row(previous_row):
    new_row = [1]

    new_length = len(previous_row) + 1

    if new_length > 1:
        new_row.extend([0] * (new_length - 2) + [1])

    for index in range(1, new_length - 1):
        new_row[index] = previous_row[index - 1] + previous_row[index]

    return new_row


def yield_triangle(num_rows):
    prev_row = []
    while len(prev_row) < num_rows:
        this_row = next_row(prev_row)
        yield this_row
        prev_row = this_row


if __name__ == "__main__":
    rows = list(yield_triangle(30))

    width_in_characters = len(rows[-1]) * 2 - 1
    for row in rows:
        print(" ".join([" " if r % 2 else "*" for r in row]).center(width_in_characters).rstrip())
