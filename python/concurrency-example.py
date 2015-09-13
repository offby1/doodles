"""This is the one use case I know of for which concurrency is
obviously a win.  We will download a pile of web pages, and find that
doing so concurrently is faster than doing them in sequence.  """

# Core
import queue
import threading

# 3rd-party
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

        # 'https://en.wikipedia.org/wiki/Eagles_(album)',
        # 'https://en.wikipedia.org/wiki/Eagles_(band)',
        # 'https://en.wikipedia.org/wiki/Eagles_(box_set)',
        # 'https://en.wikipedia.org/wiki/Eagles_cricket_team',
        # 'https://en.wikipedia.org/wiki/Eastern_Michigan_Eagles',
        # 'https://en.wikipedia.org/wiki/Eastern_Washington_Eagles',
        # 'https://en.wikipedia.org/wiki/Embry%E2%80%93Riddle_Aeronautical_University',
        # 'https://en.wikipedia.org/wiki/Fraternal_Order_of_Eagles',
        # 'https://en.wikipedia.org/wiki/Georgia_Southern_Eagles',
        # 'https://en.wikipedia.org/wiki/Germany_national_football_team',
        # 'https://en.wikipedia.org/wiki/Greg_Eagles',
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


def naive_download(urls):
    for url in urls:
        print(url, end='... ')
        requests.get(url)
        print()


def threaded_download(urls):
    result_queue = queue.Queue()
    threads = []

    class Download(threading.Thread):
        def __init__(self, url):
            self.url = url
            super(Download, self).__init__()

        def run(self):
            resp = requests.get(self.url)
            result_queue.put((self.url, len(resp.text.split())))

    for url in urls:
        t = Download(url)
        threads.append(t)
        t.start()
    for t in threads:
        t.join()

    return result_queue

if __name__ == "__main__":
    # real	0m7.146s
    # naive_download(urls)

    # real	0m1.199s
    q = threaded_download(urls)
    while not q.empty():
        print(q.get())
