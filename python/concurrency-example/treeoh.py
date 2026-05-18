# https://trio.readthedocs.io/en/stable/
import httpx
import trio


async def trio_download_one(url):
    headers = {"User-Agent": "TrioDemo/0.1 (https://gitlab.com/offby1/doodles/blob/master/python/concurrency-example)"}
    async with httpx.AsyncClient(headers=headers) as client:
        r = await client.get(url)
    text = r.text
    print(f"{url} => {len(text)} bytes")


async def trio_parent(urls):
    async with trio.open_nursery() as nursery:
        for u in urls:
            nursery.start_soon(trio_download_one, u)


def download(urls):
    trio.run(trio_parent, urls)
