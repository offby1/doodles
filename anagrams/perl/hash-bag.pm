#!/usr/bin/env perl

use warnings;
use strict;

package bag;
use Data::Dumper;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(bag bag_empty bags_equal subtract_bags);

sub bag {
  my $string = shift;
  my $bag;

  my @chars = split (qr(), $string);

  $bag->{$_}++ foreach (@chars);

  $bag;
}

sub bag_empty {
  my $bag = shift;
  0 == scalar (keys %$bag);
}

sub bags_equal {
  my $b1 = shift;
  my $b2 = shift;

  if ((scalar (keys %$b1)
       != scalar (keys %$b2))) {

    return undef;
  }

  foreach (keys %$b1) {
    if ($b1->{$_} != $b2->{$_}) {

      return undef;
    }
  }

  return 1;
}

sub subtract_bags {
  my $b1 = shift;
  my $b2 = shift;
  my $rv = undef;

  if (scalar (keys %$b2) > scalar (keys %$b1)) {

    return $rv;
  }
  foreach (keys %$b1, keys %$b2) {
    my $one = $b1->{$_} || 0;
    my $two = $b2->{$_} || 0;

    return undef
      if ($two > $one);

    $rv->{$_} = $one - $two
      unless ($one == $two);
  }

  $rv;
}

sub bag_to_string {
  my $bag = shift;
  my $rv;
  foreach (keys (%$bag)) {
    $rv .= $_ x $bag->{$_};
  }
  $rv;
}

1;
