# https://trio.readthedocs.io/en/stable/
import httpx
import trio


async def trio_download_one(url):
    async with httpx.AsyncClient() as client:
        r = await client.get(url)
    text = r.text
    print(f"{url} => {len(text)} bytes")


async def trio_parent(urls):
    async with trio.open_nursery() as nursery:
        for u in urls:
            nursery.start_soon(trio_download_one, u)


def download(urls):
    trio.run(trio_parent, urls)
