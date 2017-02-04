def next_row(previous_row=None):
    if previous_row is None:
        return [1]

    n = len(previous_row) + 1
    new_row = [0] * n

    for index in range(n):
        if index == 0:
            new_row[index] = 1
        elif index == n - 1:
            new_row[index] = 1
        else:
            new_row[index] = previous_row[index - 1] + previous_row[index]

    return new_row


def yield_triangle(num_rows):
    prev_row = None
    for row_number in range(num_rows):
        this_row = next_row(prev_row)
        yield this_row
        prev_row = this_row


num_rows = 30

if __name__ == "__main__":
    rows = []
    for row in yield_triangle(num_rows):
        rows.append(row)

    width_in_characters = len(str(rows[-1]))
    for row in rows:
        this_rows_width = len(str(row))
        left_padding = int(round((width_in_characters - this_rows_width) / 2))
        print(' ' * left_padding, end='')
        print(row)
