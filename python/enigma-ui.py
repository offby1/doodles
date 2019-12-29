from curses import wrapper
import pathlib
import random
import tempfile
import time

from enigma import Enigma


class LetterLocator:
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


def main(stdscr, transcript_binary_fh):
    stdscr.clear()

    stdscr.addstr(0, 0, "Hit ! to exit")
    stdscr.refresh()

    plaintext = []
    e = Enigma()
    locator = LetterLocator()
    last_lit_location = None

    while True:
        k = stdscr.getkey()

        if k == "!":
            break

        plaintext.append(k)
        raw_encrypted = e.encrypt_single_number(ord(k))
        if raw_encrypted:
            transcript_binary_fh.write(chr(raw_encrypted).encode("latin-1"))
            encrypted = chr(raw_encrypted)

            loc = locator(encrypted)
            if loc:

                if last_lit_location is not None:
                    stdscr.addstr(*last_lit_location, " ")
                    stdscr.refresh()

                stdscr.addstr(*loc, encrypted)
                stdscr.refresh()
                last_lit_location = loc

    transcript_binary_fh.write(b"\n")
    return plaintext


if __name__ == "__main__":
    random.seed(0)
    with tempfile.NamedTemporaryFile(delete=False) as transcript:
        plaintext = "".join(wrapper(main, transcript))
        final_name = pathlib.Path("/tmp") / plaintext
    pathlib.Path(transcript.name).rename(final_name)
    print(f"Your encryption/decryption of {plaintext!r} is in {final_name}")
