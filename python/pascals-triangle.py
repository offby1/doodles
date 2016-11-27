def _first_row():
    return [1]


def next_row(previous_row=None):
    if previous_row is None:
        return _first_row()

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


def print_triangle(num_rows):
    prev_row = None
    for row_number in range(num_rows):
        this_row = next_row(prev_row)
        print(this_row)
        prev_row = this_row


num_rows = 30

if __name__ == "__main__":
    print_triangle(num_rows)
