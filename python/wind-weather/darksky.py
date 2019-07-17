"""Collect data to prove or disprove a theory I've had for a while:

    In Seattle, when the wind comes from the North, it'll be clear for
    the next day or two.

For years I'd wanted historical weather data that included cloud cover
and wind direction.  I finally found it in DarkSky's API.

# Find the secret key at https://darksky.net/dev/account
$ DARKSKY_SECRET_KEY=xyzzy python3 darksky.py

Now I have to figure out how to tell if there's a correlation between
wind direction and cloud cover.  Maybe I'll import the data into
Google Sheets, and have it draw some sort of graph.

"""

import datetime
import os

import pytz
import requests                 # pip install requests
import tqdm                     # pip install tqdm

# See https://darksky.net/dev/docs
DARKSKY_SECRET_KEY = os.environ.get ('DARKSKY_SECRET_KEY')

TIME_MACHINE_REQUEST_URL_TEMPLATE = 'https://api.darksky.net/forecast/{key}/{latitude},{longitude},{time}'

SEATTLE_LAT_LON = (47.62052420842363, -122.34919035599756)  # decimal degrees, + is North, duh


def get_weather_for_time (time):
    url = TIME_MACHINE_REQUEST_URL_TEMPLATE.format (key=DARKSKY_SECRET_KEY,
                                                    latitude=SEATTLE_LAT_LON[0],
                                                    longitude=SEATTLE_LAT_LON[1],
                                                    time=time.isoformat ())
    response = requests.get (url)
    response.raise_for_status ()

    return response.json ()


def _24_hours_wind_and_cloud_stuff (darksky_dict):
    hourly_data = darksky_dict['hourly']['data']
    for h in hourly_data:
        try:
            yield (h['time'], h['cloudCover'], h['windBearing'])
        except KeyError:
            pass


def one_years_hourly_data (starting_year):
    print(f"Getting one year's hourly data for {starting_year}", file=sys.stderr)
    jan_1 = datetime.datetime(year=starting_year, month=1, day=1, tzinfo=pytz.utc)

    midnight = jan_1
    while midnight - jan_1 < datetime.timedelta (days=365):
        midnight += datetime.timedelta (days=1)
        for hour in _24_hours_wind_and_cloud_stuff(get_weather_for_time (midnight)):
            yield hour


def format_timestamp(time_t):
    dt = datetime.datetime.fromtimestamp(time_t, tz=pytz.utc)
    return dt.strftime('%FT%T%z')


if __name__ == '__main__':
    if DARKSKY_SECRET_KEY is None:
        raise Exception("This ain't gonna work unless you set DARKSKY_SECRET_KEY in the environment.")

    import csv
    import sys
    writer = csv.writer (sys.stdout)
    writer.writerow (('time', 'cloudCover', 'windBearing'))

    for one_hours_data in tqdm.tqdm(one_years_hourly_data (2018), total=365 * 24):
        hour = format_timestamp(one_hours_data[0])
        one_hours_data = tuple([hour]) + one_hours_data[1:]
        writer.writerow (one_hours_data)
