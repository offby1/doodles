#!/usr/bin/env perl

use warnings;
use strict;
use Data::Dumper;
use Carp;

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

sub anagrams {
  my $bag = shift;
  my $excluded_bags = shift;
  #warn "entering. bag $bag; exclusions " . Data::Dumper->Dump ([$excluded_bags], [qw(excluded_bags)]);

  my $easy_anagrams = $dict->{$bag};
  if ($easy_anagrams) {
    $easy_anagrams = [map {[$_]} @$easy_anagrams];
    $excluded_bags->{$bag}++;
    #warn "Easy anagrams of $bag: " . Dumper ($easy_anagrams) ;
    #warn Data::Dumper->Dump ([$excluded_bags], [qw(excluded_bags)]);
  } else {
    $easy_anagrams = [];
  }

  #type_check ($easy_anagrams);

  my $harder_anagrams = [];

  foreach my $pickled_dict_bag (keys %$dict) {
    #print STDERR "Looking for $pickled_dict_bag in exclusions ... ";
    if (exists $excluded_bags->{$pickled_dict_bag}) {
      #warn "Found it; moving to next dictionary entry";
      next ;
    }
    #warn "it wasn't there.";
    my $bag_from_dictionary = bag ($pickled_dict_bag);
    my $smaller_bag = subtract_bags ($bag, $bag_from_dictionary);
    next unless $smaller_bag;
    my $from_smaller_bag = anagrams ($smaller_bag, $excluded_bags);
    next unless @$from_smaller_bag;
    #die $smaller_bag .  Data::Dumper->Dump ([$from_smaller_bag], [qw(from_smaller_bag)]);
    {
      my $more;
      foreach my $short_an (@$from_smaller_bag) {
        #warn Data::Dumper->Dump ([$short_an, $pickled_dict_bag], [qw(short_an pickled_dict_bag)]);
        my @these_words = @{$dict->{$pickled_dict_bag}};
        #warn Data::Dumper->Dump ([\@these_words], [qw(these_words)]);
        my @what_is_it = map { [($_, @$short_an)]  } @these_words;
        #type_check (\@what_is_it);
        push @$more, @what_is_it;
        #warn Data::Dumper->Dump ([$more], [qw(more)]);
      }
      push @$harder_anagrams,  @$more;
    }
    #warn "Excluding $pickled_dict_bag";
    $excluded_bags->{$pickled_dict_bag}++;
  }

  #type_check ($harder_anagrams);
  my $rv = [@$easy_anagrams, @$harder_anagrams];
  #type_check ($rv);
  if (0) {
    foreach my $one_anagram (@$rv) {
      my $as_string = join ('', @$one_anagram);
      die "input size " . size ($bag) . " exceeds length of `$as_string'"
        unless (size ($bag) <= length ($as_string));
    }

    if (scalar (@$rv)) {
      #warn "Found some anagrams; here are the exclusions: " . Data::Dumper->Dump ([$excluded_bags], [qw(exclusions)]);
    }
  }
  if (0) {
    warn Data::Dumper->Dump ([$rv], [qw(so_far)])
      if (scalar (@$rv));
  }
  #warn "Returning " . join (', ', @$rv);
  return $rv;
}

{
  my $input = "Hemingway";
  my $input_as_bag = bag ($input);
  init ($input_as_bag);
  my $result = anagrams ($input_as_bag);
  print "Anagrams of $input: ",
    join (' ', map { "(" . join (' ', @$_) . ")" } @$result),
      "\n";
}
