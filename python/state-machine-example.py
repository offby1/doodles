import random


class Lock:
    def __init__(self):
        self.state = 'unlocked'

    def try_to_open(self):
        if self.state == 'locked':
            self.state = 'unlocked'
            print("Kaching!")
        elif self.state == 'unlocked':
            if random.random() < .1:
                print("Oops -- you jammed it!")
                self.state = 'jammed'
            else:
                print("Already opened.")
        elif self.state == 'jammed':
            self.state = 'locked'
            print("Maybe once more and it'll open!")
        else:
            raise Exception('wtf')

    def lock(self):
        if self.state == 'locked':
            print("Already locked")
        elif self.state == 'unlocked':
            print("Click.")
            self.state = 'locked'
        elif self.state == 'jammed':
            print("D'oh!!")
        else:
            raise Exception('wtf')

l = Lock()
for attempt in range(10):
    method = random.choice([l.try_to_open, l.lock])
    print(method.__name__, end=': ')
    method()
