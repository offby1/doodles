#!/usr/bin/env perl

use warnings;
use strict;
use Data::Dumper;
use Carp qw(confess);

use dict;
use bag;

sub type_check {
  my $thing = shift;
  confess "Not an array"
    unless ('ARRAY' eq ref ($thing));
  if (0) {
    warn "Checking " . Data::Dumper->Dump ([$thing], [qw(something)])
      if (scalar @$thing);
  }
  foreach (@$thing) {
    confess "Not an array of arrays"
      unless ('ARRAY' eq ref $_);
    confess "Not an array of arrays of strings"
      unless ($_->[0] =~ m(^[[:ascii:]]+$));
    last;
  }
}

sub excluded {
  my $bag = shift;
  confess "got a ref" if (ref ($bag));
  my @exclusions = @_;

  #print STDERR "Looking for $bag in [" , join (' ', @exclusions) , "] ... ";
  foreach (@exclusions) {
    if (bags_equal ($_, $bag)) {
      #warn "Found it; moving to next dictionary entry";
      return 1;
    }
  }

  #warn "it wasn't there.";
  return 0;
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
  my @excluded_bags = @_;
  my $rv = [];

  if (excluded ($bag, @excluded_bags)) {
    #warn " " x $l ."$bag is excluded; returning empty list";
    return $rv;
  }

  foreach my $entry (@dict) {
    my $key   = $entry->[0];
    my $words = $entry->[1];
    next if (excluded ($key, @excluded_bags));
    #warn " " x $l ."$key => [" . join (', ', @$words) . "]";

    my $smaller_bag = subtract_bags ($bag, $key);
    next unless (defined ($smaller_bag));
    #warn " " x $l ."$bag - $key = $smaller_bag";

    if (bag_empty ($smaller_bag)) {
      #warn " " x $l ."$smaller_bag is empty";
      push @excluded_bags, $key;
      my @combined = map { [$_]  } @$words;
      print join (' ', map { "(" . join (' ', @$_) . ")"} @combined), "\n" if (!$l);
      push @$rv, @combined;
    } else {
      #warn " " x $l ."$smaller_bag is not empty";
      my $from_smaller_bag = anagrams ($smaller_bag,
                                       $l + 1,
                                       @excluded_bags);
      next unless (@$from_smaller_bag);
      push @excluded_bags, $key;

      my @combined = combine ($words, $from_smaller_bag);
      push @$rv, @combined;
      print join (' ', map {"(" . join (' ', @$_) . ")"} @combined), "\n" if (!$l);
    }
    #warn "Excluding $key";
    #warn Data::Dumper->Dump ([\@excluded_bags], [qw(excluded_bags_before)]);
    #warn Data::Dumper->Dump ([\@excluded_bags], [qw(excluded_bags_after)]);
  }

  #type_check ($harder_anagrams);
  #type_check ($rv);

  #warn " " x $l ."Returning " . Data::Dumper->Dump ([$rv], [qw(rv)]);
  #die " " x $l ."Outta here" if (2 == $l);
  return $rv;
}

{
  my $input = shift;
  die "Say something!" unless defined ($input);
  my $input_as_bag = bag ($input);
  if (1) {
    init ($input_as_bag);
  } else {
    @dict = (
             ['bbboy' => [
                          'bobby',
                          'bobby'
                         ]],
             ['bo' => [
                       'ob'
                      ]],
             ['bbo' => [
                        'bob',
                        'bob'
                       ]],
             ['boy' => [
                        'boy'
                       ]]
            );
  }
  my $result = anagrams ($input_as_bag, 0);
  print scalar (@$result),
    " anagrams of $input: ",
      join (' ', map { "(" . join (' ', @$_) . ")" } @$result),
        "\n";
}
