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
import boto3                    # pip install boto3
import boto3.s3
import botocore.exceptions
import configobj                # pip install configobj
import flickrapi                # pip install flickrapi
import progressbar              # pip install progressbar2
import requests

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
            'Original': self.get_original,
        }

    def getExif(self, id_):
        return self.flickr.photos_getExif(photo_id=id_)['photo']

    def getInfo(self, id_):
        return self.flickr.photos_getInfo(photo_id=id_)['photo']

    def get_original(self, id_):
        """{u'sizes': {u'canblog': 0,
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

        for d in getSizes_response['sizes']['size']:
            if d['label'] == 'Original':
                return requests.get(d['source']).content


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
        self.bucket_name = 'flickr-sanctuary'
        self.session = None
        self.s3 = None
        self.bucket = None

    def _make_object_name(self, id_, datum_name):
        return '{}/{}'.format(id_, datum_name)

    def _object_exists(self, object_name):
        try:
            self.bucket.Object(object_name).metadata
            return True
        except botocore.exceptions.ClientError:
            return False

    def ensure_stored(self, id_, datum_name, data_thunk):
        if self.session is None:
            self.session = boto3.session.Session()

        if self.s3 is None:
            self.s3 = self.session.resource('s3')

        if self.bucket is None:
            self.bucket = self.s3.Bucket (self.bucket_name)

        objname = self._make_object_name(id_, datum_name)

        if not self._object_exists(objname):
            o = self.bucket.Object(objname)
            o.put(Body=json.dumps(data_thunk()))

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
            bar.max_value = total * len(flickr.method_map)

            id_ = photo['id']
            for datum_name, method in flickr.method_map.items():
                storage.ensure_stored(id_, datum_name, lambda : method(id_))

                bar.update(index + 1)

    except KeyboardInterrupt:
        pass
