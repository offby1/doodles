#!/usr/bin/perl

use strict;
use Data::Dumper;
use Carp qw(confess);

use dict;
use bag;

sub combine {
  my $words = shift;
  my $anagrams = shift;
  my $rv = [];

  foreach my $w (@$words) {
    push @$rv, (map {[($w, @$_)]} @$anagrams);
  }

  @$rv;
}

sub anagrams {
  my $bag = shift;
  my $l = shift;
  my @dict = @_;

  my $rv = [];

  foreach my $words_processed (0 .. $#dict) {
    my $entry = $dict[$words_processed];
    my $key   = $entry->[0];
    my $words = $entry->[1];

    my $smaller_bag = subtract_bags ($bag, $key);
    next unless (defined ($smaller_bag));

    if (bag_empty ($smaller_bag)) {
      my @combined = map { [$_]  } @$words;
      print STDERR join (' ', map { "(" . join (' ', @$_) . ")"} @combined), "\n" if (!$l);
      push @$rv, @combined;
    } else {
      my $from_smaller_bag = anagrams ($smaller_bag,
                                       $l + 1,
                                       @dict[$words_processed .. $#dict]);
      next unless (@$from_smaller_bag);

      my @combined = combine ($words, $from_smaller_bag);
      push @$rv, @combined;
      print STDERR join (' ', map {"(" . join (' ', @$_) . ")"} @combined), "\n" if (!$l);
    }
  }

  return $rv;
}

{
  my $input = shift;
  die "Say something!" unless defined ($input);
  my $input_as_bag = bag ($input);
  init ($input_as_bag);

  my $result = anagrams ($input_as_bag, 0, @dict);
  print STDERR scalar (@$result),
    " anagrams of $input\n";
  print join (' ', map { "(" . join (' ', @$_) . ")" } @$result),
        "\n";
}
