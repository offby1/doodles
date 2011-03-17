#!/usr/bin/env python

"""Some documentation would be nice!"""

from __future__ import print_function
from __future__ import absolute_import

__author__ = 'erich@cozi.com'

from pyramid.config import Configurator
from pyramid.response import Response
from paste.httpserver import serve

import StringIO
import pprint

def hello_world(request):
    resp_body = 'Hello world!<br/><pre>{0}</pre>'.format(request)
    s = StringIO.StringIO()
    pprint.pprint(request.urlvars, stream=s)
    resp_body += s.getvalue()
    return Response(resp_body)

def goodbye_world(request):
    return Response('Goodbye world!')

if __name__ == '__main__':
    config = Configurator()
    config.add_view(hello_world)
    config.add_view(goodbye_world, name='goodbye')
    app = config.make_wsgi_app()
    serve(app, host='0.0.0.0')
