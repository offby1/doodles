#!/usr/bin/env perl

use warnings;
use strict;

package dict;
use Carp qw(cluck);
use Data::Dumper;
use bag;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw($dict init);

our $dict;

my $dict_file_name = "/usr/share/dict/words";

sub init {
  my $bag = shift;

  if (!-r "dict.pl") {
    # Open this first, so that if it fails we won't have wasted time
    # reading a dictionary that we cannot cache.
    open (PICKLE, ">", "dict.pl")
      or die "Can't open dict.pl for writing: $!; stopped";

    open (DICT, "<", $dict_file_name)
      or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

    print STDERR "Reading $dict_file_name ...";

    while (<DICT>) {
      chomp;
      next unless $_;

      my $b = bag ($_);
      if (0) {

        if (0) {
          next if (m([^[:alpha:]]));
          next if (m([[:upper:]]));
          next unless ((m(^i$)i)
                       ||
                       (m(^a$)i)
                       ||
                       (m(^..)));
          next unless (m([aeiou]));
        } else {
          next unless (subtract_bags ($bag, $b));
        }
      }

      {
        my $entry = $dict->{$b};
        if (defined ($entry)) {
          my %existing_entries = map { ($_, 1)} @{$entry};
          #die "$_: " . Data::Dumper->Dump ([$dict], [qw(dict)]);
          if (exists $existing_entries{$_}) {
            #warn "skipping duplicate entry $_: $existing_entries{$_}";
            next;
          }
        }
      }
      #die "entering $_; dict is now " . Data::Dumper->Dump ([$dict], [qw(dict)]) if ($. > 10);
      push @{$dict->{$b}}, lc ($_);
    }
    close (DICT)
      or die "Can't close filehandle: $!; stopped";
    warn " done.  Dictionary hath " . scalar (keys %$dict) . " elements";

    print PICKLE Data::Dumper->Dump ([$dict], [qw(dict)]);
    close (PICKLE)
      or warn "Can't close filehandle: $!";
  }

  do "dict.pl";
  warn "Before pruning: " . %$dict;
  while ((my $k, my $v) = (each %$dict)) {
    my $makeable = subtract_bags ($bag, $k);
    #cluck "$bag minus $k is $makeable";
    if ($makeable) {
      #warn "Keeping key `$k', value `@$v'";
    } else {
      delete $dict->{$k};
    }
  }
  warn "After pruning: " . %$dict;

}
1;
