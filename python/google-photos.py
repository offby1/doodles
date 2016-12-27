# to install dependencies:
# $ git clone https://github.com/google/gdata-python-client.git
# $ python2 setup.py install
# $ pip2 install tlslite

import gdata.photos.service
import gdata.media
import gdata.geo

"""

So ... I'd like to learn how to upload photos to Google Photos.
Sounds like it should be simple, right?  Noooo.

There doesn't appear to be a Google Photos API.  There is, however, a
picasaweb API: https://developers.google.com/picasa-web/.  That page
says "Beginning May 1st, 2016, we’ll start rolling out changes to the
Picasa Web Albums Data API and no longer support the following
functionality" Last I checked, it was December 2016.  Everything about
this site seems abandoned.

There's a python API for "version 1" (the docs suggest there's a
stable version 2, and a "draft" version 3, but no python clients for
either).  The python client's tutorial tells me to use a version of
OAuth that seems obsolete and unsupported.

My remaining hope is that I can upload photos to some sort of generic
data-uploading API such as https://cloud.google.com/storage/, which as
best I can tell from the marketing gibberish, is roughly equivalent to
Amazon's S3.  There appears to be a python client at
https://github.com/GoogleCloudPlatform/google-cloud-python.

The Google Services docs are so various and complex that it's
daunting.  I keep putting off reading them.

"""

# From https://developers.google.com/picasa-web/docs/1.0/developers_guide_python
def GetAuthSubUrl():
    # My little ec2 box running a temporary web server
    next = 'http://ec2-52-8-12-207.us-west-1.compute.amazonaws.com:8000/'

    scope = 'https://picasaweb.google.com/data/'
    secure = False
    session = True
    gd_client = gdata.photos.service.PhotosService()
    return gd_client.GenerateAuthSubURL(next, scope, secure, session);

authSubUrl = GetAuthSubUrl();
# Crap, this URL leads to https://support.google.com/a/answer/162106
# which basically says "This flavor of OUath hasn't been supported for
# years; get a horse"
print '<a href="%s">Login to your Google account</a>' % authSubUrl
