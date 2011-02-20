#!/usr/bin/env python

"""
A sort of "hello world" web server.
"""

from __future__ import print_function
from __future__ import absolute_import

__author__ = 'erich@cozi.com'

from webob.dec import wsgify
import wsgiref.simple_server

class Simple(object):
    def __init__(self):
        self.counter = 0
    
    @wsgify
    def __call__(self, req):
        self.counter += 1
        return "I have been called {self.counter} times".format(**locals())

if __name__ == "__main__":
    httpd = wsgiref.simple_server.make_server('localhost', 8000, Simple())
    httpd.serve_forever()
