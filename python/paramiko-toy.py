import codecs
import paramiko                 # pip install paramiko
import pathlib
import sys


def from_named_file(fn):
    with open(fn, 'r') as inf:
        for ctor in (paramiko.rsakey.RSAKey.from_private_key, paramiko.dsskey.DSSKey.from_private_key):
            try:
                return ctor(inf)
            except (paramiko.ssh_exception.SSHException, IndexError):
                pass


def from_named_dir(d):
    for fn in d.rglob('*'):
        if not fn.is_file():
            continue
        try:
            yield fn, from_named_file(fn)
        except (paramiko.ssh_exception.PasswordRequiredException,
                paramiko.ssh_exception.SSHException,
                UnicodeDecodeError) as e:
            print('{}: {}'.format(fn, e), file=sys.stderr)


def key2str(pk):
    return '{} bits'.format(pk.get_bits())


if __name__ == "__main__":
    for name, result in from_named_dir(pathlib.Path("~/.ssh").expanduser()):
        if result:
            print('{}: {}'.format(name, key2str(result)))
        else:
            print(f"Nuts, {name} doesn't seem to be a proper key")

    agent = paramiko.agent.Agent()
    for pk in agent.get_keys():
        print('{}: {}'.format(codecs.encode(pk.get_fingerprint(), 'hex'), key2str(pk)))
