#!/usr/bin/env perl

use warnings;
use strict;

package bag;

use Carp qw(cluck confess);
use Data::Dumper;
use Math::BigInt lib => 'GMP';
warn "using " . Math::BigInt->config()->{lib};

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(bag bag_empty bags_equal subtract_bags  size);

my @primes = qw(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101) ;

{
  my $a_code = ord ("a");
  sub char_to_factor {
    my $char = lc (shift);

    if ($char !~ m([[:alpha:]])) {
      return 1;
    }

    $primes [ord ($char) - $a_code];
  }
}

sub bag {
  my $thing = shift;

  confess "I was passed a reference"
    if (ref ($thing));

  my $product = Math::BigInt->new(1);

  foreach (split (qr(), $thing)) {
    my $factor = char_to_factor ($_);
    $product *=  $factor;
  }

  return $product;
}

sub bag_empty {
  1 == shift;
}

sub bags_equal {
  shift == shift;
}

sub subtract_bags {
  my $b1 = shift;
  my $b2 = shift;

  if (0 == ($b1 % $b2)) {
    return $b1 / $b2;
  }

  undef;
}

die "bag_empty"
  unless (bag_empty (bag ("")));

die "bag_empty"
  if (bag_empty (bag ("a")));

die "didn't ignore a space"
  unless (bags_equal (bag ("a "),
                     (bag ("a"))));

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

die "Unpickling"
  unless (bags_equal (bag ("cat"), 2 * 5 * 71));

print "We cool!\n";
1;
