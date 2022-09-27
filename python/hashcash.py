"""
From https://en.wikipedia.org/wiki/Hashcash

       X-Hashcash: 1:20:1303030600:adam@cypherspace.org::McMybZIhxKXu57jd:ckvi

The header contains:
ver      : Hashcash format version, 1 (which supersedes version 0).
bits     : Number of "partial pre-image" (zero) bits in the hashed code.
date     : The time that the message was sent, in the format YYMMDD[hhmm[ss]].
resource : Resource data string being transmitted, e.g., an IP address or email address.
ext      : Extension (optional; ignored in version 1).
rand     : String of random characters, encoded in base-64 format.
counter  : Binary counter, encoded in base-64 format.
"""

import base64
import datetime
import hashlib
import itertools
import os
import subprocess


def _assemble_bytes_from_components(random_bytes, counter):
    ver = 1
    bits = 20
    resource = "frotz@plotz.com"
    ext = ""

    date = datetime.datetime.utcnow().strftime("%y%m%d%H%M%S")
    rand_b64 = base64.b64encode(random_bytes).decode("utf-8")
    counter_b64 = base64.b64encode(integer_to_bytes(counter)).decode("utf-8")
    return f"{ver}:{bits}:{date}:{resource}:{ext}:{rand_b64}:{counter_b64}".encode(
        "utf-8"
    )


def _leading_zeroes_of_byte(b):
    if b == 0:
        return 8
    rv = 0
    while b < 128:
        rv += 1
        b *= 2
    return rv


def _leading_zeroes_of_bytes(bytes_):
    rv = 0
    for b in bytes_:
        this_count = _leading_zeroes_of_byte(b)
        rv += this_count
        if this_count < 8:
            break
    return rv


def integer_to_bytes(i):
    rv = []
    while i > 0:
        q, r = divmod(i, 256)
        rv.append(r)
        i = q
    return bytes(reversed(rv))


def find_string_whose_hash_has_leading_zeroes():
    most_leading_zeroes_seen = 0
    random_bytes = os.urandom(10)

    for count in itertools.count(1):
        candidate = _assemble_bytes_from_components(random_bytes, count)

        leading_zeroes, is_legit = validate_candidate(candidate)

        if is_legit:
            return candidate

        if leading_zeroes > most_leading_zeroes_seen:
            most_leading_zeroes_seen = leading_zeroes
            print(f'{candidate.decode("utf-8")} => {leading_zeroes}')

    return None


def validate_candidate(bytes_):
    needed_leading_zeroes = int(bytes_.split(b":", 2)[1])
    hashed = hashlib.sha1(bytes_).digest()
    leading_zeroes = _leading_zeroes_of_bytes(hashed)
    if leading_zeroes >= needed_leading_zeroes:
        str_ = bytes_.decode("utf-8")
        print(f"{str_} is a legit hashcash thingy")

        bash_command_line = f"echo -n {str_} | openssl sha1"
        sha1 = subprocess.run(["bash", "-c", bash_command_line], stdout=subprocess.PIPE)
        print(f'{bash_command_line!r} => {sha1.stdout.rstrip().decode("utf-8")}')
        print(f"That has {leading_zeroes} leading zero bits.")
        return leading_zeroes, True

    return leading_zeroes, False


if __name__ == "__main__":
    find_string_whose_hash_has_leading_zeroes()
