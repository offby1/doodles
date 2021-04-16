import random
from tkinter import E, EventType, N, S, Tk, W, ttk

from enigma import Enigma


def on_key_press(*events):
    global ciphertext

    event = events[0]
    if event.type == EventType.KeyPress:
        letter = event.char

        encrypted = enigma.encrypt_single_letter(letter)
        if encrypted:
            label = labels_by_letter[encrypted]
            label.configure(foreground='yellow')

            ciphertext += encrypted
            ciphertext_label.configure(text=ciphertext[-30:])

            plaintext = plaintext_label.cget('text')
            plaintext += letter
            plaintext_label.configure(text=plaintext[-30:])


def on_key_release(*events):
    event = events[0]
    if event.type == EventType.KeyRelease:
        for label in labels_by_letter.values():
            label.configure(foreground='black')


labels_by_letter = {}


def add_keyboard(parent_window):
    def generic_label(column_index, row_index, text):
        label = ttk.Label(parent_window, text=text)
        label.grid(
            column=column_index,
            row=row_index,
            sticky=(N, S, E, W),
        )
        label.configure(
            foreground='black'
        )
        parent_window.columnconfigure(column_index, weight=1)
        parent_window.rowconfigure(row_index, weight=1)
        return label

    keyboard_layout = [
        "q w e r t y u i o p",
        " a s d f g h j k l",
        "  z x c v b n m",
    ]

    letters_to_place = set(enigma.alphabet)

    for row_index, row_letters in enumerate(keyboard_layout):
        for column_index, letter in enumerate(row_letters):

            if letter == ' ':
                continue

            label = generic_label(column_index, row_index, letter)
            labels_by_letter[letter] = label
            root.bind(f'<KeyPress-{letter}>', lambda e: on_key_press(e))
            root.bind(f'<KeyRelease-{letter}>', lambda e: on_key_release(e))

            letters_to_place.remove(letter)

    for index, letter in enumerate(sorted(letters_to_place)):
        label = generic_label(column_index, row_index + 1, '_' if letter == ' ' else letter)
        labels_by_letter[letter] = label
        root.bind(f'<KeyPress-{letter}>', lambda e: on_key_press(e))
        root.bind(f'<KeyRelease-{letter}>', lambda e: on_key_release(e))


def on_output_label_Destroy(e):
    # printing to stdout seems not to work; I assume the event loop has done something weird with it
    with open('ciphertext', 'w') as outf:
        print(ciphertext, file=outf)
        print(f"Wrote {outf.name}")


random.seed('')
enigma = Enigma()
ciphertext = ''

root = Tk()
root.title("Enigma Encryptor")

input_frame = ttk.Frame(root)
input_frame.grid(column=0, row=0, sticky=(N, W, E, S))

output_frame = ttk.Frame(root)
output_frame.grid(column=0, row=1, sticky=(W, S))

plaintext_label = ttk.Label(output_frame, text='')
plaintext_label.grid(column=0, row=0, sticky=(W, S))
plaintext_label['font'] = 'TkFixedFont'

ciphertext_label = ttk.Label(output_frame, text='')
ciphertext_label.grid(column=0, row=1, sticky=(W, S))
ciphertext_label['font'] = 'TkFixedFont'
ciphertext_label.bind('<Destroy>', on_output_label_Destroy)

root.columnconfigure(0, weight=1)
root.rowconfigure(0, weight=1)

add_keyboard(input_frame)
root.mainloop()
