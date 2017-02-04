from itertools import count
from channel import Channel, Loop

import click                    # pip install click


async def generate(ch):
    for i in count(2):
        await ch.transmit(i)


async def filter_(in_, out, prime):
    while True:
        i = await in_.receive()
        if i % prime != 0:
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


@click.command()
@click.argument('number_of_primes', type=int, default=5)
def print_n_primes(number_of_primes):
    loop = Loop()
    loop.run_until_complete(main(number_of_primes, loop))


if __name__ == "__main__":
    print_n_primes()
