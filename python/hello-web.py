#!/usr/bin/env python3

"""
A sort of "hello world" web server.
"""

# Core
from collections import defaultdict

# 3rd-party
from webob import Response      # pip install webob
from webob.dec import wsgify
import wsgiref.simple_server


class Simple(object):

    def __init__(self):
        self.counts_by_URL = defaultdict(int)

    @wsgify
    def __call__(self, req):
        self.counts_by_URL[req.path] += 1
        return Response("My call history: " + str(dict(self.counts_by_URL)),
                        content_type='text/plain')


if __name__ == "__main__":
    host = 'localhost'
    port = 8000
    httpd = wsgiref.simple_server.make_server(host, port, Simple())
    print("Point yon web browser at http://{host}:{port}".format(host=host, port=port))
    httpd.serve_forever()
