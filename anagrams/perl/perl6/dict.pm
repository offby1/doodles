#!/usr/local/src/langs/pugs/pugs

use bag;

our @dict;

# I originally had `our %dict_hash', which works exactly the same
# way. But this will, supposedly in a future version of pugs, limit
# the hash keys to Ints, which might improve performance, or provide
# error-checking, or something.
our Any %dict_hash{Int};

#my $dict_file_name = "/usr/share/dict/words";
my $dict_file_name = "words";

sub acceptable (Str $word) returns Bool {
  if ($word ~~ rx:perl5{[^[:alpha:]]}) {
    return Bool::False ;
  }

  if ($word ~~ "i") {
    return Bool::True ;
  }

  if ($word ~~ "a") {
    return Bool::True ;
  }

  if ($word.chars < 2) {
    return Bool::False ;
  }

  if ($word ~~ rx:perl5{[aeiou]}) {
    return Bool::True ;
  }

  return Bool::False;
}

sub snarf_wordlist {
  my $dict = open($dict_file_name, :r)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  print $*ERR: "Reading $dict_file_name ...";

  for ($dict.readline) -> $word {
                                 my $chopped = lc (chomp($word));
                                 next unless (acceptable($chopped));
                                 %dict_hash{bag($chopped)}.push($chopped);
                                };
  print $*ERR: " done\n";
  close ($dict) or die "Closing $dict: $!";
}

snarf_wordlist();
{
  my $cache_file_name = "dict.cache";
  my $cache = open($cache_file_name, :w)
    or die "Can't open $cache_file_name for writing 'cuz $!; stopped";
  $cache.print(%dict_hash.perl);
  close ($cache) or die "Closing $cache";
}
1;
