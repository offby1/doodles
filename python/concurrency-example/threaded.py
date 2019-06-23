import threading
import queue

import common


def download(urls):
    result_queue = queue.Queue()
    threads = []

    class Download(threading.Thread):
        def __init__(self, url):
            self.url = url
            super(Download, self).__init__()

        def run(self):
            result_queue.put(common.download_one(self.url))

    for url in urls:
        t = Download(url)
        threads.append(t)
        t.start()
    for t in threads:
        t.join()

    while not result_queue.empty():
        print(result_queue.get())
