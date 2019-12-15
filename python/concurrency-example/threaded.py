import threading
import queue

import common


def download(urls):
    result_queue = queue.Queue()

    class Download(threading.Thread):
        def __init__(self, url):
            self.url = url
            super(Download, self).__init__()

        def run(self):
            result_queue.put(common.download_one(self.url))

    threads = [Download(url) for url in urls]

    for t in threads:
        t.start()

    for t in threads:
        t.join()

    while not result_queue.empty():
        print(result_queue.get())
