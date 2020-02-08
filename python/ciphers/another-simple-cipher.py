import random

bytes_ = bytes(range(256))
scrambled = bytes(random.sample(bytes_, k=len(bytes_)))

encrypt = dict(zip(bytes_, scrambled))
decrypt = dict(zip(scrambled, bytes_))


def transform_bytes(bytes_, map_):
    return bytes([map_.get(b, b) for b in bytes_])


with open("/etc/passwd", "rb") as inf:
    plaintext = inf.read()

    with open("/tmp/decrypt-me", "wb") as outf:
        outf.write(transform_bytes(plaintext, encrypt))
        print(f"Check out {outf.name!r} for some ciphertext")

    with open("/tmp/decrypt-me", "rb") as inf:
        print(transform_bytes(inf.read(), decrypt).decode("utf-8"))
