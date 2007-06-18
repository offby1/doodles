#!/usr/bin/env perl

use warnings;
use strict;
use Data::Dumper;

binmode STDOUT, ":utf8";

foreach (0 .. 0x110000) {
  next if (0xd800 <= $_) && ($_ <= 0xdfff);
  print chr($_), "\n";
}
