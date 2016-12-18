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

def _size_list_to_dict(getSizes_response):
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
    rv = {}
    for d in getSizes_response['sizes']['size']:
        rv[d['label']] = d

    return {'sizes': rv}


def _unpaginated_search(flickr, user_id):
    requested_page = 1
    per_page = 100

    while True:
        rsp = flickr.photos_search(user_id=user_id,
                                   page=requested_page,
                                   per_page=str(per_page))

        photos = rsp['photos']

        for photo in photos['photo']:
            yield int(photos['total']), photo

        if int(photos['page']) == int(photos['pages']):
            return

        requested_page += 1


class PhotoStore:
    filename = '/tmp/photos.js'

    def __init__(self, filename=None):
        self.photos_by_id = {}
        if filename is not None:
            self.filename = filename

        try:
            with open(self.filename) as inf:
                self.photos_by_id = json.load(inf)
        except ValueError:
            os.unlink(self.filename)
        except IOError as e:
            print(e)
        else:
            print("Loaded {} items from {}".format(len(self.photos_by_id), self.filename))


    def save_to_file(self):
        with open(self.filename, 'w') as outf:
            json.dump(self.photos_by_id, outf, indent=1)
        print("Saved {} items to {}".format(len(self.photos_by_id), self.filename))


    def has_id(self, id_):
        return id_ in self.photos_by_id


    def store_by_id(self, id_, data):
        self.photos_by_id[id_] = data


api_key, shared_secret = get_auth_stuff()

flickr = flickrapi.FlickrAPI(api_key,
                             shared_secret,
                             format='parsed-json',
                             cache=True)

my_nsid = flickr.people_findByUsername(username='offby1')['user']['nsid']

ps = PhotoStore()

with progressbar.ProgressBar() as bar:
    try:
        for index, (total, photo) in enumerate(_unpaginated_search(flickr, my_nsid)):
            bar.max_value = total

            id_ = photo['id']
            if not ps.has_id(id_):
                blob = {}
                blob.update(flickr.photos_getExif(photo_id=id_)['photo'])
                blob.update(flickr.photos_getInfo(photo_id=id_)['photo'])
                blob.update(_size_list_to_dict(flickr.photos_getSizes(photo_id=id_)))
                ps.store_by_id(id_, blob)

            bar.update(index)

            # TODO -- persist the blob somewhere, using id_ as an index.

            # TODO -- then skip most of the stuff if we find that
            # we've already persisted this id_.

    except KeyboardInterrupt:
        pass

ps.save_to_file()
