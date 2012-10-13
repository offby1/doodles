#!/usr/bin/env perl

# http://programmingpraxis.com/2012/10/12/birthday-paradox/
# also
#

use Modern::Perl;

use Data::Dumper; $Data::Dumper::Sortkeys = 1;

sub generate_birthdays {
  my ($population_size) = @_;

  map {int(rand (365))} (1..$population_size);
}

sub one_trial {
  my ($population_size) = @_;

  my @folks = generate_birthdays ($population_size);
  my $bday_to_count;
  foreach (@folks) {
    $bday_to_count->{$_}++;
  }

  foreach (values %$bday_to_count) {
    if ($_ gt 1) {
      return 1;
    }
  }

  return 0;
}

foreach my $pop (1..50) {
  my $trials = 100;
  my $wins = 0;
  foreach my $trial (1..$trials) {
    $wins++ if (one_trial ($pop));
  }

  printf "Population %d; %d trials; win likelihood %f\n",
    $pop, $trials, $wins / $trials;
}
