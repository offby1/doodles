#!/usr/bin/perl

use strict;
use Data::Dumper;
use Carp qw(confess);

use dict;
use bag;

sub excluded {
  my $bag = shift;
  my $exclusions = shift;

  #warn Data::Dumper->Dump ([$bag, $exclusions], [qw(bag exclusions)]);
  my $rv = exists ($exclusions->{$bag});
  #warn "returning $rv";
  return $rv;
}

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
  my $original_exclusions = shift;

  # A deep copy of the original exclusions.  We'll be modifying this
  # copy, but we don't want to modify the original.
  my $excluded_bags;
  $excluded_bags = {(%$original_exclusions)}
    if (defined ($original_exclusions));

  my $rv = [];

  return $rv if (excluded ($bag, $excluded_bags));

  foreach my $entry (@dict) {
    my $key   = $entry->[0];
    my $words = $entry->[1];

    next if (excluded ($key, $excluded_bags));

    my $smaller_bag = subtract_bags ($bag, $key);
    next unless (defined ($smaller_bag));

    if (bag_empty ($smaller_bag)) {
      $excluded_bags->{$key}++;
      my @combined = map { [$_]  } @$words;
      print STDERR join (' ', map { "(" . join (' ', @$_) . ")"} @combined), "\n" if (!$l);
      push @$rv, @combined;
    } else {
      my $from_smaller_bag = anagrams ($smaller_bag,
                                       $l + 1,
                                       $excluded_bags);
      next unless (@$from_smaller_bag);
      $excluded_bags->{$key}++;

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

  my $result = anagrams ($input_as_bag, 0);
  print scalar (@$result),
    " anagrams of $input: ",
      join (' ', map { "(" . join (' ', @$_) . ")" } @$result),
        "\n";
}
