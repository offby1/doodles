#!/usr/bin/env perl

use warnings;
use strict;

package bag;

use Carp qw(cluck);
use Data::Dumper;
use Math::BigInt lib => 'GMP';
warn "using " . Math::BigInt->config()->{lib};

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(bag bag_empty bags_equal subtract_bags bag_to_string size);

my @primes = map { Math::BigInt->new ($_)} qw(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101);

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

  my $product = 1;
  my $size = 0;

  return $thing if ('ARRAY' eq ref ($thing));

  if ($thing =~ m((\d+), (\d+))) {
    $product = $1;
    $size = $2;
  } else {

    my @chars = split (qr(), $thing);

    foreach (@chars) {
      my $factor = char_to_factor ($_);
      $product *=  $factor;
      $size ++ if ($factor > 1);
    }
  }
  return [$product, $size];
}

sub product {
  $_[0]->[0];
}

sub size {
  $_[0]->[1];
}

sub bag_empty {
  my $bag = shift;

  return 1
    if !defined ($bag);

   0 == size ($bag);
}

sub bags_equal {
  my $b1 = shift;
  my $b2 = shift;
  my $diff = subtract_bags ($b1, $b2);

  bag_empty ($diff);
}

sub subtract_bags {
  my $b1 = shift;
  my $b2 = shift;

  return undef
    if (
        (size ($b2) > size ($b1))
        ||
        (product ($b1) % product ($b2)));

  my $quotient = product ($b1) / product ($b2);
  my $size = size ($b1) - size ($b2);

  return [$quotient, $size];
}

sub bag_to_string {
  my $bag = shift;
  product ($bag) . ", " . size ($bag);
}

1;
