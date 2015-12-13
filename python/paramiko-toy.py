import os.path
import paramiko


def from_named_file(fn):
    with open(fn, 'r') as inf:
        try:
            return paramiko.rsakey.RSAKey.from_private_key(inf)
        except paramiko.ssh_exception.SSHException:
            return paramiko.dsskey.DSSKey.from_private_key(inf)


def from_named_dir(d):
    for fn in os.scandir(d):
        p = fn.path
        try:
            yield p, from_named_file(p)
        except (paramiko.ssh_exception.PasswordRequiredException,
                paramiko.ssh_exception.SSHException,
                UnicodeDecodeError) as e:
            print('{}: {}'.format(p, e))

if __name__ == "__main__":
    for name, result in from_named_dir(os.path.expanduser("~/.ssh")):
        print('{}: {} bits'.format(name, result.get_bits()))
