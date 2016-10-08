"""This is the one use case I know of for which concurrency is
obviously a win.  We will download a pile of web pages, and find that
doing so concurrently is faster than doing them in sequence.  """

# Core
import asyncio
import concurrent.futures
import contextlib
import queue
import signal
import sys
import threading

# 3rd-party
import aiohttp                  # pip install aiohttp
import requests                 # pip install requests

urls = ('https://en.wikipedia.org/wiki/American_Eagles',
        'https://en.wikipedia.org/wiki/Ateneo_Blue_Eagles',
        'https://en.wikipedia.org/wiki/Bedford_Town_F.C.',
        'https://en.wikipedia.org/wiki/Boston_College_Eagles',
        'https://en.wikipedia.org/wiki/Chris_Eagles',
        'https://en.wikipedia.org/wiki/Colorado_Eagles',
        'https://en.wikipedia.org/wiki/Coppin_State_Eagles',
        'https://en.wikipedia.org/wiki/Crystal_Palace_F.C.',
        'https://en.wikipedia.org/wiki/Eagle_(disambiguation)',
        'https://en.wikipedia.org/wiki/Eagles_(1984_film)',
        'https://en.wikipedia.org/wiki/Eagles_(2012_film)',

        'https://en.wikipedia.org/wiki/Eagles_(album)',
        'https://en.wikipedia.org/wiki/Eagles_(band)',
        'https://en.wikipedia.org/wiki/Eagles_(box_set)',
        'https://en.wikipedia.org/wiki/Eagles_cricket_team',
        'https://en.wikipedia.org/wiki/Eastern_Michigan_Eagles',
        'https://en.wikipedia.org/wiki/Eastern_Washington_Eagles',
        'https://en.wikipedia.org/wiki/Embry%E2%80%93Riddle_Aeronautical_University',
        'https://en.wikipedia.org/wiki/Fraternal_Order_of_Eagles',
        'https://en.wikipedia.org/wiki/Georgia_Southern_Eagles',
        'https://en.wikipedia.org/wiki/Germany_national_football_team',
        'https://en.wikipedia.org/wiki/Greg_Eagles',

        # 'https://en.wikipedia.org/wiki/Hanwha_Eagles',
        # 'https://en.wikipedia.org/wiki/Jeanne_Eagels',
        # 'https://en.wikipedia.org/wiki/Manly-Warringah_Sea_Eagles',
        # 'https://en.wikipedia.org/wiki/Marquette_Golden_Eagles',
        # 'https://en.wikipedia.org/wiki/Morehead_State_Eagles',
        # 'https://en.wikipedia.org/wiki/Niagara_Purple_Eagles',
        # 'https://en.wikipedia.org/wiki/North_Texas_Eagles',
        # 'https://en.wikipedia.org/wiki/Northern_Eagles',
        # 'https://en.wikipedia.org/wiki/Oral_Roberts_Golden_Eagles',
        # 'https://en.wikipedia.org/wiki/P.A.O.K.',
        # 'https://en.wikipedia.org/wiki/PFC_Ludogorets_Razgrad',
        # 'https://en.wikipedia.org/wiki/Philadelphia_Eagles',
        # 'https://en.wikipedia.org/wiki/S.L._Benfica',
        # 'https://en.wikipedia.org/wiki/SWD_Eagles',
        # 'https://en.wikipedia.org/wiki/Sheffield_Eagles',
        # 'https://en.wikipedia.org/wiki/Southern_Miss_Golden_Eagles',
        # 'https://en.wikipedia.org/wiki/Surrey_Eagles',
        # 'https://en.wikipedia.org/wiki/The_Eagles_(UK_band)',
        # 'https://en.wikipedia.org/wiki/The_Eagles_(rhythm_and_blues_group)',
        # 'https://en.wikipedia.org/wiki/Tohoku_Rakuten_Golden_Eagles',
        # 'https://en.wikipedia.org/wiki/USA_Eagles',
        # 'https://en.wikipedia.org/wiki/USCGC_Eagle_(WIX-327)',
        # 'https://en.wikipedia.org/wiki/Washington_Eagles',
        # 'https://en.wikipedia.org/wiki/West_Coast_Eagles',
        # 'https://en.wikipedia.org/wiki/Winthrop_Eagles',
        # 'https://en.wikipedia.org/wiki/Woodville-West_Torrens_Eagles',
)

#urls=urls[0:3]


def download_one(url):
    return '{} => {} bytes'.format(url, len(requests.get(url).text))


def naive_download(urls):
    for url in urls:
        print(download_one(url))


def threaded_download(urls):
    result_queue = queue.Queue()
    threads = []

    class Download(threading.Thread):
        def __init__(self, url):
            self.url = url
            super(Download, self).__init__()

        def run(self):
            result_queue.put(download_one(self.url))

    for url in urls:
        t = Download(url)
        threads.append(t)
        t.start()
    for t in threads:
        t.join()

    while not result_queue.empty():
        print(result_queue.get())


def future_download(urls):
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(urls)) as executor:
        for result in executor.map(download_one, urls):
            print(result)


async def async_download_one(client, url):
    async with client.get(url) as response:
        assert response.status == 200
        text = await response.text()
    print('{} => {} bytes'.format(url, len(text)))


def async_download(urls):
    with contextlib.closing(asyncio.get_event_loop()) as loop:
        with contextlib.closing(aiohttp.ClientSession(loop=loop)) as client:

            def signal_handler(signal, frame):
                loop.stop()
                client.close()
                sys.exit(0)

            signal.signal(signal.SIGINT, signal_handler)

            tasks = [asyncio.ensure_future(async_download_one(client, u)) for u in urls]

            loop.run_until_complete(asyncio.gather(*tasks))


if __name__ == "__main__":
    import timeit
    def t(description, python_expression):
        print(description)
        print(timeit.timeit(python_expression, globals=globals(), number=1))


    t("naive:"   , 'naive_download(urls)')
    t("threaded:", 'threaded_download(urls)')
    t("futures:" , 'future_download(urls)')
    t("asyncio:" , 'async_download(urls)')
