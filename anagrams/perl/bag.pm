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
  my $rv = $thing;

  unless ($thing =~ m(^(\d+), (\d+)$)) {

    my $product = 1;
    my $size = 0;

    foreach (split (qr(), $thing)) {
      my $factor = char_to_factor ($_);
      $product *=  $factor;
      $size ++ if ($factor > 1);
    }
    $rv = "$product, $size";
  }
  confess "Empty bag!" unless ($rv =~ m(, [1-9]));
  return $rv;
}

sub product {
  my $b = shift;
  my $rv = "1";
  if (defined ($b)) {
    $rv = Math::BigInt->new ((split (m(,), $b))[0]);
  }

  $rv;
}

sub size {
  my $b = shift;
  my $rv = "0";
  if (defined ($b)) {
    $rv = (split (m(, +), $b))[1];
  }

  $rv;
}

sub bag_empty {
  my $b = shift;
  my $rv = "1";

  if (defined ($b)) {
    $rv = ((0 == size ($b)) ? "1" : "0");
  }

  return $rv;
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
  my $rv = undef;

  #confess "Why you be subtracting nothing?" unless (size ($b2));

  my $size = size ($b1) - size ($b2);

  if (($size > 0)
      &&
      (0 == (product ($b1) % product ($b2)))) {

    my $quotient = product ($b1) / product ($b2);
    $rv = "$quotient, $size";
  }

  $rv;
}

1;
