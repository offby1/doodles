import requests


def download_one(url):
    return '{} => {} bytes'.format(url, len(requests.get(url).text))
