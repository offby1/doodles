#!/usr/bin/env perl

use warnings;
use strict;
use Data::Dumper;

use bag;

die "bag_empty"
  unless (bag_empty (undef));

die "bag_empty"
  unless (bag_empty (bag ("")));

die "bag_empty"
  if (bag_empty (bag ("a")));

die "bags_equal"
  unless (bags_equal (bag ("abc"),
                      bag ("cba"))) ;

die "bags_equal"
  if (bags_equal (bag ("abc"),
                  bag ("bc")));

die "subtract_bags"
  unless (bags_equal (bag ("a"),
                      subtract_bags (bag("ab"),
                                     bag ("b")))) ;

die "subtract_bags"
  if (subtract_bags (bag ("a"),
                     bag ("b")));

die "subtract_bags"
  if (subtract_bags (bag ("a"),
                     bag ("aa")));

{
  my $silly_long_string = <<EOF;
When first I was a wee, wee lad
Eating on my horse
I had to take a farting duck
Much to my remorse.
Oh Sally can't you hear my plea
When Roe V Wade is nigh
And candles slide the misty morn
With dimples on your tie.
EOF

  my $ever_so_slightly_longer_string = $silly_long_string . "x";
  die "subtract_bags"
    unless (bags_equal (bag ("x"),
                        subtract_bags (bag ($ever_so_slightly_longer_string),
                                       bag ($silly_long_string))));
}

# Only for the numeric implementation of bags
{
  my $hmm = bag (2 * 5 * 73 . ", " . 3);

  die "Unpickling"
    unless (bags_equal (bag ("cat"), $hmm));
}
print "We cool!\n";
