import requests


def download_one(url):
    return f"{url} => {len(requests.get(url).text)} bytes"
