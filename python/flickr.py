#!/usr/bin/env python

"""Some documentation would be nice!"""

from __future__ import print_function
from __future__ import absolute_import

__author__ = 'eric.hanchrow@gmail.com'

# Core
import collections
import datetime
import os
import pprint
import sys

# 3rd-party
import configobj                # pip install configobj
import flickrapi                # pip install flickrapi


def get_auth_stuff(filename=None):
    if filename is None:
        filename = os.path.expanduser('~/.flickr-auth')

    c = configobj.ConfigObj(filename)

    return(c['flickr']['api_key'],
           c['flickr']['shared_secret'])

api_key, shared_secret = get_auth_stuff()

flickr = flickrapi.FlickrAPI(api_key, shared_secret)


def dump(thing, indent=0):
    print(' ' * indent, end='')
    print(thing.__class__.__name__,
          repr(thing.tag),
          repr(thing.text),
          thing.items())
    for sub in thing:
        dump(sub, indent + 2)

my_nsid = flickr.people_findByUsername(username='offby1').find('user').attrib['nsid']
id_to_exif_tag_to_exif_value = collections.defaultdict(dict)

requested_page = 1
while True:
    rsp = flickr.photos_search(user_id=my_nsid,
                               page=requested_page,
                               per_page='10',
                               min_upload_date=datetime.datetime(2010, 12, 1))
    photos = rsp.find('photos')

    if photos.get('pages') == '0':
        print("Hmm, no photos at all.")
        break

    print("This is page", photos.get('page'), "of", photos.get('pages'), file=sys.stderr)
    for photo in photos:
        id = photo.get('id')
        print(photo.get('title'), id, "...", file=sys.stderr)
        for exif in flickr.photos_getExif(api_key=api_key,
                                          photo_id=id,
                                          secret=shared_secret).find('photo').findall('exif'):
            id_to_exif_tag_to_exif_value[id][exif.get('tag')] = exif.find('raw').text

    if int(photos.get('page')) >= int(photos.get('pages')):
        print("That's all!")
        break

    requested_page += 1

pprint.pprint(dict(id_to_exif_tag_to_exif_value))
# sets = flickr.photosets_getList(user_id='73509078@N00')
