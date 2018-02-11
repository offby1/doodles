import collections
import csv
import itertools
import sys

reader = csv.DictReader (sys.stdin)
next(reader)

covers_by_bearing = collections.defaultdict (list)

current_reader, advanced_reader = itertools.tee (reader)
advanced_reader = itertools.islice (advanced_reader, 24, None)

for _tuple, advanced_tuple in zip (current_reader, advanced_reader):
    windBearing = int(_tuple['windBearing'])
    cloudCover  = float(_tuple['cloudCover'])
    advancedCloudCover  = float(advanced_tuple['cloudCover'])

    covers_by_bearing[windBearing].append (advancedCloudCover)

writer = csv.writer (sys.stdout)
writer.writerow (('windBearing (degrees)', 'cloudCover'))
for bearing, covers in sorted (covers_by_bearing.items ()):
    for c in sorted(covers):
        writer.writerow ((bearing, c))
