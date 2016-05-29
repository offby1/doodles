import queue
from threading import Thread

def fire_and_forget(callable):
    T = Thread(target=callable)
    T.start()


def integers(sink):
    i = 2
    while True:
        sink.put(i)
        i += 1


def filter_sequence(source, sink, predicate):
    while True:
        item = source.get()
        if predicate(item):
            sink.put(item)


def is_multiple_of(x, factor):
    return (x % factor) == 0


q = queue.Queue()
fire_and_forget(lambda : integers(q))

while True:
    prime = q.get()
    print(prime)

    if prime > 2000:
        break

    q2 = queue.Queue()
    fire_and_forget(lambda : filter_sequence(q, q2, lambda x: not (is_multiple_of(x, prime))))
    q = q2
