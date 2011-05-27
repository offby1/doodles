#!/usr/bin/env python

"""
Mess with amazon's simpleDB service via some Python library or other.
"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

__author__ = 'eric.hanchrow@gmail.com'

import os.path
import ConfigParser
import simpledb
import time

if __name__ == "__main__":
    my_credentials = ConfigParser.SafeConfigParser()
    with open(os.path.expanduser("~/.aws_creds")) as fp:
        my_credentials.readfp(fp)

    sdb = simpledb.SimpleDB(my_credentials.get('aws', 'access key id'),
                            my_credentials.get('aws', 'Secret Access Key'))

    frotz_domain = sdb['frotz']

    frotz_domain['test'] = dict(action="a value with spaces",
                                snorgulous=u'an ellipsis:\u2026',
                                frotz=u'a nasty Unicode character:\ufffd',
                                )
    time.sleep(2)


# git clone http://github.com/sixapart/python-simpledb.git
# cd python-simpledb
# python setup.py build

# Local Variables:
# compile-command: "PYTHONPATH=/usr/local/src/python-simpledb/build/lib.linux-i686-2.7 python sdb-fiddling.py "
# End:
