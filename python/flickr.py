#!/usr/bin/env python

from __future__ import print_function
from __future__ import absolute_import

# Core
import datetime
import os
import pprint
import sys

# 3rd-party
import configobj                # pip install configobj
import flickrapi                # pip install flickrapi
import progressbar              # pip install progressbar2

__author__ = 'eric.hanchrow@gmail.com'


def get_auth_stuff(filename=None):
    if filename is None:
        filename = os.path.expanduser('~/.flickr-auth')

    c = configobj.ConfigObj(filename)

    return(c['flickr']['api_key'],
           c['flickr']['shared_secret'])

api_key, shared_secret = get_auth_stuff()

flickr = flickrapi.FlickrAPI(api_key,
                             shared_secret,
                             format='parsed-json',
                             cache=True)

my_nsid = flickr.people_findByUsername(username='offby1')['user']['nsid']

requested_page = 1
with progressbar.ProgressBar(max_value=progressbar.UnknownLength) as bar:
    while True:
        rsp = flickr.photos_search(user_id=my_nsid,
                                   page=requested_page,
                                   per_page='10')

        photos = rsp['photos']

        for photo in photos['photo']:
            id = photo['id']
            info = flickr.photos_getInfo(photo_id=id)
            pprint.pprint(info)
            bar.update()

        if int(photos['page']) >= int(photos['pages']):
            print("That's all!")
            break

        requested_page += 1
