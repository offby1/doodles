def _first_row():
    return [1]


def nth_row(n):
    if n == 1:
        return _first_row()

    previous_row = nth_row(n - 1)
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
    for row_number in range(1, num_rows + 1):
        print(nth_row(row_number))


num_rows = 10

if __name__ == "__main__":
    print_triangle(num_rows)
