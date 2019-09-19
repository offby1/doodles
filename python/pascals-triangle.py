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
    for _ in range(num_rows):
        this_row = next_row(prev_row)
        yield this_row
        prev_row = this_row


if __name__ == "__main__":
    rows = []
    for row in yield_triangle(30):
        rows.append(row)

    width_in_characters = len(str(rows[-1]))
    for row in rows:
        this_rows_width = len(str(row))
        left_padding = int(round((width_in_characters - this_rows_width) / 2))
        print(' ' * left_padding, end='')
        print(row)
