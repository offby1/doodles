#!/usr/bin/env python

from __future__ import print_function
from __future__ import absolute_import

# Core
import datetime
import json
import os
import pprint
import sys

# 3rd-party
import configobj                # pip install configobj
import flickrapi                # pip install flickrapi
import progressbar              # pip install progressbar2


# API docs: https://www.flickr.com/services/api/

def get_auth_stuff(filename=None):
    if filename is None:
        filename = os.path.expanduser('~/.flickr-auth')

    c = configobj.ConfigObj(filename)

    return(c['flickr']['api_key'],
           c['flickr']['shared_secret'])


class FlickrAdapter:
    def __init__(self, flickr, username='offby1'):
        self.flickr = flickr
        self.username = username
        self.method_map = {
            'Exif':  self.getExif,
            'Info':  self.getInfo,
            'Sizes': self.getSizes,
        }

    def getExif(self, id_):
        return self.flickr.photos_getExif(photo_id=id_)['photo']

    def getInfo(self, id_):
        return self.flickr.photos_getInfo(photo_id=id_)['photo']

    def getSizes(self, id_):
        """For mysterious reasons, the bulk of this information is a list of
        dicts, rather than a dict keyed on the name of the size.

        {u'sizes': {u'canblog': 0,
                    u'candownload': 1,
                    u'canprint': 0,
                    u'size': [{u'height': 75,
                               u'label': u'Square',
                               u'media': u'photo',
                               u'source': u'https://farm9.staticflickr.com/8326/29332283952_f830b0e681_s.jpg',
                               u'url': u'https://www.flickr.com/photos/offby1/29332283952/sizes/sq/',
                               u'width': 75},
                              {u'height': u'150',
                               u'label': u'Large Square',
                               u'media': u'photo',
                               u'source': u'https://farm9.staticflickr.com/8326/29332283952_f830b0e681_q.jpg',
                               u'url': u'https://www.flickr.com/photos/offby1/29332283952/sizes/q/',
                               u'width': u'150'}]},
         u'stat': u'ok'}
        """
        getSizes_response = self.flickr.photos_getSizes(photo_id=id_)
        rv = {}
        for d in getSizes_response['sizes']['size']:
            rv[d['label']] = d

        return {'sizes': rv}


    def all_photo_metadata(self):
        requested_page = 1
        per_page = 100

        my_nsid = self.flickr.people_findByUsername(username=self.username)['user']['nsid']

        while True:
            rsp = self.flickr.photos_search(user_id=my_nsid,
                                            page=requested_page,
                                            per_page=str(per_page))

            photos = rsp['photos']

            for photo in photos['photo']:
                yield int(photos['total']), photo

            if int(photos['page']) == int(photos['pages']):
                return

            requested_page += 1

class Storage:
    def __init__(self):
        self.container = os.path.join(os.path.dirname(__file__), 'storage')

    def _make_fn(self, id_, datum_name):
        return os.path.join(self.container, id_, datum_name)

    def has_datum(self, id_, datum_name):
        fn = self._make_fn(id_, datum_name)
        return os.path.isfile(fn)

    def store_datum(self, id_, datum_name, data, force=True):
        fn = self._make_fn(id_, datum_name)
        try:
            os.makedirs(os.path.dirname(fn))
        except OSError:
            pass

        if force or not self.has_datum(id_, datum_name):
            with open(fn, 'w') as outf:
                json.dump(data, outf)


if __name__ == "__main__":
    api_key, shared_secret = get_auth_stuff()

    flickr = FlickrAdapter(flickrapi.FlickrAPI(api_key,
                                               shared_secret,
                                               format='parsed-json',
                                               cache=True))


    storage = Storage()

    bar = progressbar.ProgressBar()
    bar.start()

    try:
        for index, (total, photo) in enumerate(flickr.all_photo_metadata()):
            bar.max_value = total

            id_ = photo['id']
            for datum_name, method in flickr.method_map.items():
                if not storage.has_datum(id_, datum_name):
                    storage.store_datum(id_, datum_name, method(id_))

                bar.update(index)

    except KeyboardInterrupt:
        pass
