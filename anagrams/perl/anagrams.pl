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
  warn "Checking " . Data::Dumper->Dump ([$thing], [qw(something)])
    if (scalar @$thing);
  foreach (@$thing) {
    confess "Not an array of arrays"
      unless ('ARRAY' eq ref $_);
    confess "Not an array of arrays of strings"
      unless ($_->[0] =~ m(^[[:ascii:]]+$));
    last;
  }
}

sub anagrams {
  my $input = shift;
  my $bag = bag ($input);
  my $excluded_bags = shift;

  my $easy_anagrams = $dict->{bag_to_string ($bag)};
  if ($easy_anagrams) {
    $easy_anagrams = [map {[$_]} @$easy_anagrams];
    $excluded_bags->{bag_to_string ($bag)}++;
    #warn "Easy anagrams of " . bag_to_string ($bag) . ": " . Dumper ($easy_anagrams) ;
    #warn Data::Dumper->Dump ([$excluded_bags], [qw(excluded_bags)]);
  } else {
    $easy_anagrams = [];
  }

  #type_check ($easy_anagrams);

  my $harder_anagrams = [];

  foreach my $pickled_dict_bag (keys %$dict) {
    next if (exists $excluded_bags->{$pickled_dict_bag});
    my $bag_from_dictionary = bag ($pickled_dict_bag);
    my $smaller_bag = subtract_bags ($bag, $bag_from_dictionary);
    next unless $smaller_bag;
    my $from_smaller_bag = anagrams ($smaller_bag, $excluded_bags);
    next unless @$from_smaller_bag;
    #die bag_to_string ($smaller_bag) .  Data::Dumper->Dump ([$from_smaller_bag], [qw(from_smaller_bag)]);
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
      die "input size " . size ($bag) . " differs from length of `$as_string'"
        unless (size ($bag) == length ($as_string));
    }

    if (scalar (@$rv)) {
      warn "Found some anagrams; here are the exclusions: " . Data::Dumper->Dump ([$excluded_bags], [qw(exclusions)]);
    }
  }
  warn Data::Dumper->Dump ([$rv], [qw(so_far)])
    if (scalar (@$rv));
  return $rv;
}

{
  my $input = "EricSam";
  my $result = anagrams ($input);
  print "Anagrams of $input: ", Data::Dumper->Dump ([$result], [$input]);
}
