import collections
from dataclasses import dataclass
import pprint
import statistics
import sys

@dataclass
class Entry:
    key: str
    text: str

    def __str__(self):
        return f'\t{self.key}) {self.text}'

class Menu:
    def __init__(self, entries):
        self.entries = {e.key: e for e in entries}

    def display(self):
        sorted_entries = sorted(self.entries.items())
        return '\n'.join([str(e[1]) for e in sorted_entries])

    def determine_choice(self, input_string):
        return self.entries.get(input_string.strip())

    def force_valid_choice(self, string_source):
        while True:
            raw = string_source()
            c = self.determine_choice(raw)

            if c is None:
                print(f"C'mon now, be a good boy: {raw!r} isn't in {self.entries.keys()}")
            else:
                return c

def test_empty_menu_all_choices_None():
    m = Menu([])
    assert m.determine_choice('') is None
    assert m.determine_choice('whatever') is None

def test_single_entry_choice():
    e = Entry(key='key', text='text')
    m = Menu([e])
    assert m.determine_choice('key') is e
    assert m.determine_choice('waah') is None

def test_single_entry_display():
    e = Entry(key='key', text='text')
    m = Menu([e])
    assert m.display() == '\tkey) text'

def test_multi_entry_display_is_sorted():
    m = Menu([
        Entry(key='Zooey', text='zebra'),
        Entry(key='Arnold', text='aardvark'),
    ])
    assert m.display() == '\tArnold) aardvark\n\tZooey) zebra'

def choose_continue_or_quit():
    """
    Returns False if they want to quit; True otherwise.
    """
    m = Menu([
        Entry(key='1', text='Enter student information'),
        Entry(key='2', text='Quit'),
    ])

    print(m.display())
    c = m.force_valid_choice(input)

    if c.key == '2':
        return False

    return True


def collect_valid_test_score():
    while True:
        wat = input("Enter a grade level here: ")

        try:
            i = int(wat)
        except ValueError as e:
            print(f'Nope: {e}')
        else:
            if 0 <= i <= 100:
                return i
            print(f'Nope: {i} is too big or too small')

def choose_grade_level():
    m = Menu([
        Entry(key='1', text='Freshman'),
        Entry(key='2', text='Sophomore'),
        Entry(key='3', text='Junior'),
        Entry(key='4', text='Senior'),
    ])
    print(m.display())
    return m.force_valid_choice(input)

if __name__ == "__main__":
    counts_by_grade = collections.Counter()
    test_scores = []
    while choose_continue_or_quit():
        grade = choose_grade_level()
        counts_by_grade[grade.text] += 1
        test_scores.append(collect_valid_test_score())

    print(f"Total number of students entered: {len(test_scores)}")
    if len(test_scores):
        print(f"Average test score: {statistics.mean(test_scores)}")
    pprint.pprint(dict(counts_by_grade))

"""
:-) 2018-07-07T21:30:15-0700 [Eric-Hanchrows-MacBook-Pro tmp]$ (echo 1; echo 2; echo 50; echo 1; echo 1; echo 24; echo 1; echo 4; echo 99; echo 2)| python3 menu.py
	1) Enter student information
	2) Quit
	1) Freshman
	2) Sophomore
	3) Junior
	4) Senior
Enter a grade level here: 	1) Enter student information
	2) Quit
	1) Freshman
	2) Sophomore
	3) Junior
	4) Senior
Enter a grade level here: 	1) Enter student information
	2) Quit
	1) Freshman
	2) Sophomore
	3) Junior
	4) Senior
Enter a grade level here: 	1) Enter student information
	2) Quit
Total number of students entered: 3
Average test score: 57.666666666666664
{'Freshman': 1, 'Senior': 1, 'Sophomore': 1}
"""
