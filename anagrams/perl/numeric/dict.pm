#!/usr/bin/env perl

use warnings;
use strict;

package dict;
use Carp qw(cluck carp);
use Data::Dumper;
use bag;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(@dict init);

our @dict;
our $dict_hash;
my $dict_file_name = "/usr/share/dict/words";
my $dict_hash_name = "bigus-dictus.pl";

sub maybe_snarf_wordlist {

  if (!-r $dict_hash_name) {

    open (HASH, ">", $dict_hash_name)
      or die "Oh hell, can't write to $dict_hash_name: $!";

    open (DICT, "<", $dict_file_name)
      or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

    print STDERR "Reading $dict_file_name ...";

    while (<DICT>) {
      chomp;
      next unless $_;

      $_ = lc ($_);
      my $b = bag ($_);

      next if (m([^[:alpha:]]));
      next unless ((m(^i$))
                   ||
                   (m(^a$))
                   ||
                   (m(^..)));
      next unless (m([aeiou]));

      my $entry = $dict_hash->{$b};

      if (defined ($entry)) {
        my $duplicate;
        foreach my $existing_entry (@$entry) {
          if ($_ eq $existing_entry) {
            $duplicate = 1;
            last;
          }
        }

        next if ($duplicate);
      }

      #die "entering $_; dict is now " . Data::Dumper->Dump ([$dict], [qw(dict)]) if ($. > 10);
      push @{$dict_hash->{"$b"}}, $_;
    }
    close (DICT)
      or die "Can't close filehandle: $!; stopped";

    print HASH Data::Dumper->Dump ([$dict_hash], [qw(dict_hash)]);

    close (HASH)
      or warn "Can't close filehandle: $!";
  }

  do $dict_hash_name;
}

sub init {
  my $filter = shift;
  carp "bag is `$filter'";

  maybe_snarf_wordlist;

  @dict = ();
  warn "Before pruning: " . scalar (keys %$dict_hash);
  while ((my $k, my $v) = (each %$dict_hash)) {
    $k = Math::BigInt->new($k);
    my $makeable = subtract_bags ($filter, $k);

    if (defined ($makeable)) {
      push @dict, [($k, [sort (@$v)])];
    }
  }

  # just for fun, let's sort the dictionary.  Biggest words first.
  @dict = (sort {length ($b->[0])
                   <=>
                     length ($a->[0])} @dict);

  warn "After pruning and sorting: " . scalar (@dict);
}
1;
