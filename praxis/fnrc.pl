#!/usr/bin/perl

# http://programmingpraxis.com/2011/08/19/first-non-repeating-character/

use warnings;
use strict;

use Data::Dumper; $Data::Dumper::Sortkeys = 1;
use Test::More qw(no_plan);

sub first_non_repeated_char {
  my ($string) = @_;

  my %char_to_stats;

  my $offset = 0;
  foreach (split(//, $string)) {
    $char_to_stats{$_} ||= [$offset, 0];
    $char_to_stats{$_}->[1]++;
    $offset++;
  }

  my $offset_of_first_unique = 1 + length($string);
  my $first_unique;

  foreach my $char (keys %char_to_stats) {
    my $offset = $char_to_stats{$char}->[0];
    my $count  = $char_to_stats{$char}->[1];

    next if $count > 1;
    if ($offset < $offset_of_first_unique) {
      $offset_of_first_unique = $offset;
      $first_unique = $char;
    }
  }
  $first_unique;
}

is(first_non_repeated_char(''), undef);
is(first_non_repeated_char('abc'), 'a');
is(first_non_repeated_char('abca'), 'b');
is(first_non_repeated_char('aabcbcdeef'), 'd');
