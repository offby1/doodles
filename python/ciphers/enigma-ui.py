"""python3 enigma-ui.py frotz && pbpaste | python3 enigma.py frotz

TODO:

- do something sensible when backspace key is pressed.  This will require making the enigma go backwards.

- do something sensible when Command-V is pressed (or really, the platform's native "paste" keystroke): synthesize
  keypresses corresponding to what's in the clipboard.

- draw the actual wheels, somehow, so we can watch them turn around as we type.

"""

import random
import sys
from tkinter import E, EventType, N, S, StringVar, Tk, W, font, ttk

from enigma import Enigma

labels_by_letter = {}


def on_key_press(*events):
    event = events[0]
    if event.type == EventType.KeyPress:
        letter = event.char

        if event.state != 0:  # ignore typing when modifier keys are pressed
            return

        encrypted = enigma.encrypt_single_letter(letter)
        if encrypted:
            for label in labels_by_letter.values():
                label.configure(foreground='black')

            label = labels_by_letter[encrypted]
            label.configure(foreground='yellow')

            ciphertext_stringvar.set(ciphertext_stringvar.get() + encrypted)
            ciphertext_entry.xview('end')  # ensures the text scrolls to the left as needed

            ciphertext_entry.clipboard_clear()
            ciphertext_entry.clipboard_append(ciphertext_stringvar.get())

            plaintext = plaintext_label.cget('text')
            plaintext += letter
            plaintext_label.configure(text=plaintext[-20:])


def add_letter_displays(parent_window):
    display_layout = [
        "q-w-e-r-t-y-u-i-o-p",
        "-a-s-d-f-g-h-j-k-l",
        "--z-x-c-v-b-n-m",
        "------ --.",
    ]

    for row_index, row_letters in enumerate(display_layout):
        for column_index, letter in enumerate(row_letters):

            if letter == '-':
                continue

            label = ttk.Label(
                parent_window,
                # space is invisible; this is easier to see
                text=('_' if letter == ' ' else letter),
                font=the_font,
            )
            label.grid(
                column=column_index,
                row=row_index,
                sticky=(N, S, E, W),
            )
            label.configure(foreground='black')
            parent_window.columnconfigure(column_index, weight=1)
            parent_window.rowconfigure(row_index, weight=1)

            labels_by_letter[letter] = label
            root.bind(f'<KeyPress-{letter}>', lambda e: on_key_press(e))


secret_key = ''
if len(sys.argv) > 1:
    secret_key = sys.argv[1]

random.seed(secret_key)
enigma = Enigma()

root = Tk()
root.title("Enigma Encryptor")

ciphertext_stringvar = StringVar()

the_font = font.nametofont('TkFixedFont')
the_font.configure(size=75)

input_frame = ttk.Frame(root)
input_frame.grid(column=0, row=0, sticky=(N, W, E, S))

output_frame = ttk.Frame(root)
output_frame.grid(column=0, row=1, sticky=(W, E, S))

plaintext_label = ttk.Label(output_frame, text='', font=the_font)
plaintext_label.grid(column=0, row=0, sticky=(W, E, S))

ciphertext_entry = ttk.Entry(
    output_frame, textvariable=ciphertext_stringvar, font=the_font, state='readonly'
)
ciphertext_entry.grid(column=0, row=1, sticky=(W, E, S))

root.columnconfigure(0, weight=1)
root.rowconfigure(0, weight=1)

add_letter_displays(input_frame)
root.mainloop()
