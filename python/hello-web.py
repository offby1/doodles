#!/usr/bin/env python

"""
A sort of "hello world" web server.
"""

from __future__ import print_function
from __future__ import absolute_import

__author__ = 'erich@cozi.com'

from collections import defaultdict
from webob import Response
from webob.dec import wsgify
import wsgiref.simple_server

class Simple(object):
    def __init__(self):
        self.counts_by_URL = defaultdict(int)
    
    @wsgify
    def __call__(self, req):
        self.counts_by_URL[req.path] += 1
        return Response("My call history: " + str(self.counts_by_URL),
                        content_type='text/plain')

if __name__ == "__main__":
    httpd = wsgiref.simple_server.make_server('localhost', 8000, Simple())
    httpd.serve_forever()
