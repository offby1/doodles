import concurrent.futures

import common


def download(urls):
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(urls)) as executor:
        for result in executor.map(common.download_one, urls):
            print(result)
