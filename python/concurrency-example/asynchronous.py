import asyncio
import contextlib
import logging
import signal
import sys

import aiohttp  # pip install aiohttp

logging.basicConfig(level=logging.INFO)


async def async_download_one(aiohttp_client, url):
    async with aiohttp_client.get(url) as response:
        assert response.status == 200
        text = await response.text()
    print('{} => {} bytes'.format(url, len(text)))


async def create_aiohttp_session(loop):
    return aiohttp.ClientSession(loop=loop)


def download(urls):
    with contextlib.closing(asyncio.get_event_loop()) as loop:
        loop.set_debug(enabled=True)
        client = loop.run_until_complete(create_aiohttp_session(loop))

        def signal_handler(signal, frame):
            loop.stop()
            client.close()
            sys.exit(0)

        signal.signal(signal.SIGINT, signal_handler)

        tasks = [asyncio.ensure_future(async_download_one(client, u)) for u in urls]

        loop.run_until_complete(asyncio.gather(*tasks))
        loop.run_until_complete(client.close())
