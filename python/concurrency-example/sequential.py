import common


def download(urls):
    for url in urls:
        print(common.download_one(url))
