#!/usr/bin/env perl

use warnings;
use strict;

package dict;
use Data::Dumper;
use bag;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw($dict);

our $dict;

my $dict_file_name = "/usr/share/dict/words";

if (-r "dict.pl") {
  warn "Snarfing existing dict.pl";
  do "dict.pl";
} else {
  # Open this first, so that if it fails we won't have wasted time
  # reading a dictionary that we cannot cache.
  open (PICKLE, ">", "dict.pl")
    or die "Can't open dict.pl for writing: $!; stopped";

  open (DICT, "<", $dict_file_name)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  print STDERR "Reading $dict_file_name ...";

  while (<DICT>) {
    chomp;

    next if (m([^[:alpha:]]));
    next if (m([[:upper:]]));
    next unless (m(^..));
    next unless (m([aeiou]));

    push @{$dict->{bag_to_string (bag ($_))}}, $_;
  }
  close (DICT)
    or die "Can't close filehandle: $!; stopped";
  warn " done.  Dictionary hath " . scalar (keys %$dict) . " elements";

  print PICKLE Data::Dumper->Dump ([$dict], [qw(dict)]);
  close (PICKLE)
    or warn "Can't close filehandle: $!";
}
1;
