from collections import deque, namedtuple
import types

class Channel:
    def __init__(self, name='', size=1):
        self.deque = deque(maxlen=size)
        self.name = name

    @types.coroutine
    def transmit(self, message):
        yield (self, "transmit")
        self.deque.append(message)

    @types.coroutine
    def receive(self):
        yield (self, "receive")
        return self.deque.popleft()

    def full(self):
        return len(self.deque) == self.deque.maxlen

    def empty(self):
        return len(self.deque) == 0

    def __repr__(self):
        return self.name


Waiting = namedtuple('Waiting', "function channel mode")

ready_transmit = lambda w: w.mode == 'transmit' and not w.channel.full()
ready_receive = lambda w: w.mode == 'receive' and not w.channel.empty()

class Loop:
    def __init__(self):
        self.pending = set()

    def run(self, f):
        v = f.send(None)
        self.pending.add(Waiting(f, *v))

    def _pick(self):

        candidates = set(w for w in self.pending if ready_transmit(w) or ready_receive(w))
        try:
            pick = candidates.pop()
        except KeyError:
            raise Exception('deadlock!')
        else:
            self.pending.remove(pick)

        return pick


    def run_until_complete(self, f):
        self.run(f)
        while any(w.function is f for w in self.pending):
            pick = self._pick()
            try:
                self.run(pick.function)
            except StopIteration:
                pass
