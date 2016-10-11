import collections
import concurrent.futures
import logging
import queue
import random
import string
import time
import timeit

logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)-15s %(levelname)s [%(thread)d] %(message)s')

_log = logging.getLogger(__name__)

"""Simulate a particular use for concurrency:

We've got a server that does one main chunk of work on demand.  That
chunk is moderately expensive (i.e., it takes maybe 20 ms to finish).
We want that chunk to do a second piece of work as well -- something
I/O bound, whose result doesn't affect the result of the main work
(think of writing to a log file, or notifying an interested party via
HTTP or something).  That second piece of work also takes perhaps 30ms.

We'd like both pieces of work to proceed in parallel, so that the main
chunk isn't noticeably delayed.

"""

# This queue isn't a necessary part of the simulation; it's here only
# to assure me that the notifications aren't being lost (they aren't
# :-)
notification_queue = queue.Queue()


def do_one_notification(datum, i):
    time.sleep(0.03)
    if i == 0:
        _log.debug("Notified for %s.%d", datum, i)
    notification_queue.put((datum, i))


def main_work(datum, executor):
    time.sleep(0.02)

    # In reality, we'd only send one notification per unit of work.
    # But sending a pile of them makes it more likely that we'll
    # notice extra time being spent somewhere it oughtn't.
    num_notes = 100
    _log.debug("Sending %d notifications for %s", num_notes, datum)
    for i in range(num_notes):
        executor.submit(do_one_notification, datum, i)


def harness(num_times, executor):
    """ Invoke main_work NUM_TIMES times, with a different random datum each time."""
    for _ in range(num_times):
        inp = ''.join(random.choices (4, string.ascii_lowercase))
        main_work(inp, executor)


if __name__ == "__main__":
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        print(timeit.timeit('harness(10, executor)', globals=globals(), number=1))
        _log.info("Done with timeit; waiting for futures, I guess")

    _log.info("Done waiting for futures; collecting notifications.")
    notifications_by_datum = collections.defaultdict(list)
    while not notification_queue.empty():
        datum, index = notification_queue.get()
        notifications_by_datum[datum].append(index)

    print('datum: number of notifications received')
    for datum, notes in notifications_by_datum.items():
        print('{}: {}'.format(datum, len(notes)))
