#!/bin/sh

libtoolize --automake
AUTOMAKE="automake --add-missing --foreign" autoreconf
