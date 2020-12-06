import asyncio
import contextlib


async def print_some_stuff(stuff):
    for _ in range(4):
        print(f"{stuff} here")
        await asyncio.sleep(1)


with contextlib.closing(asyncio.get_event_loop()) as loop:
    loop.set_debug(enabled=True)
    tasks = [asyncio.ensure_future(print_some_stuff(stuff)) for stuff in ('first thing', 'second thing')]

    loop.run_until_complete(asyncio.gather(*tasks))

# python3 async-toy.py
# first thing here
# second thing here
# first thing here
# second thing here
# first thing here
# second thing here
# first thing here
# second thing here
