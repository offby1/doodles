from curses import wrapper
import random

from enigma import Enigma


class LetterLocator:
    """
    Given a letter, figure out where to display it on the screen.
    """
    def __init__(self):
        self.locations_by_letter = {}
        keyboard_layout = [
            "",
            "",
            "",
            "q w e r t y u i o p",
            " a s d f g h j k l",
            "  z x c v b n m",
        ]

        for row_index, row_letters in enumerate(keyboard_layout):
            for column_index, letter in enumerate(row_letters):
                self.locations_by_letter[letter] = (row_index, column_index)

    def __call__(self, l):
        return self.locations_by_letter.get(l)


def main(stdscr):
    stdscr.clear()

    stdscr.addstr(0, 0, "Hit ! to exit")
    stdscr.refresh()

    plaintext = []
    ciphertext = []
    e = Enigma()
    locator = LetterLocator()
    last_lit_location = None

    while True:
        k = stdscr.getkey()

        # display whatever key they pressed in the upper-left, for debugging.
        # erase the area first.
        stdscr.addstr(1, 0, ' ' * 10)
        stdscr.addstr(1, 0, k)

        if k == "!":
            break

        plaintext.append(k)
        try:
            encrypted = e.encrypt_single_number(ord(k.lower()))
        except TypeError:
            # k might have been e.g. "KEY_RIGHT", on which ord will puke.
            encrypted = None

        if encrypted:
            encrypted = chr(encrypted)
            ciphertext.append(encrypted)

            loc = locator(encrypted)
            if loc:

                if last_lit_location is not None:
                    stdscr.addstr(*last_lit_location, " ")

                stdscr.addstr(*loc, encrypted)
                stdscr.refresh()
                last_lit_location = loc

    return ''.join(plaintext), ''.join(ciphertext)


if __name__ == "__main__":
    random.seed("")
    plaintext, ciphertext = wrapper(main)

    print(ciphertext)
