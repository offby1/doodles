import contextlib
import signal
import sys

import aiohttp  # pip install aiohttp
import asyncio


async def async_download_one(client, url):
    async with client.get(url) as response:
        assert response.status == 200
        text = await response.text()
    print('{} => {} bytes'.format(url, len(text)))


def download(urls):
    with contextlib.closing(asyncio.get_event_loop()) as loop:
        with contextlib.closing(aiohttp.ClientSession(loop=loop)) as client:

            def signal_handler(signal, frame):
                loop.stop()
                client.close()
                sys.exit(0)

            signal.signal(signal.SIGINT, signal_handler)

            tasks = [asyncio.ensure_future(async_download_one(client, u)) for u in urls]

            loop.run_until_complete(asyncio.gather(*tasks))
