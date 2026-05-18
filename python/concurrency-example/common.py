import requests

HEADERS = {"User-Agent": "ConcurrencyDemo/0.1 (https://gitlab.com/offby1/doodles/blob/master/python/concurrency-example)"}


def download_one(url):
    return f"{url} => {len(requests.get(url, headers=HEADERS).text)} bytes"
