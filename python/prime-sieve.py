from itertools import count
import sys
from channel import Channel, Loop

async def generate(ch):
    for i in count(2):
        await ch.transmit(i)

async def filter_(in_, out, prime):
    while True:
        i = await in_.receive()
        if i%prime != 0:
            await out.transmit(i)

async def main(n, loop):
    ch = Channel()
    loop.run(generate(ch))
    for i in range(n):
        prime = await ch.receive()
        print(prime)
        ch1 = Channel()
        loop.run(filter_(ch, ch1, prime))
        ch = ch1

if __name__ == '__main__':
    loop = Loop()
    n = int(sys.argv[1])
    loop.run_until_complete(main(n, loop))
