import csv
import sys

reader = csv.DictReader (sys.stdin)
next(reader)

pairs = []
for datum in reader:
    cloudCover = datum['cloudCover']
    windBearing = datum['windBearing']

    pairs.append ((windBearing, cloudCover))

writer = csv.writer (sys.stdout)
writer.writerow (('windBearing (degrees)', 'cloudCover'))
for p in sorted (pairs, key=lambda p: int (p[0])):
    writer.writerow (p)
