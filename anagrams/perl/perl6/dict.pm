#!/usr/local/src/langs/pugs/pugs
module dict;
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

# Don't read more than this many elements from the dictionary.  Useful
# only for debugging, since reading the whole dictionary is really
# really slow.  And note that it doesn't count lines read from the
# file, nor does it count values stored in the hash; rather, it counts
# the number of distinct keys in the hash.
my $max_size = 1000;

sub snarf_wordlist {
  my $dict = open($dict_file_name, :r)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  print $*ERR: "Reading $dict_file_name ...";

  for ($dict.readline) -> $word {
                                 my $chopped = lc (chomp($word));
                                 next unless (acceptable($chopped));
                                 %dict_hash{bag($chopped)}.push($chopped);
                                 if (%dict_hash.elems == $max_size) {
                                   say "$max_size is enough elements; won't read no mo'";
                                   last;
                                 }
                                };
  print $*ERR: " done\n";
  close ($dict) or die "Closing $dict: $!";
}

my $cache_file_name = "dict.cache";
if (-f $cache_file_name) {
#  %dict_hash = open("dict.cache").slurp.eval;
  %dict_hash = open("dict.cache").slurp.eval(:lang<yaml>);
  say "Slurped $cache_file_name";
} else {
  say "Slurping word list ...";
  snarf_wordlist();
  {
    my $cache = open($cache_file_name, :w)
      or die "Can't open $cache_file_name for writing 'cuz $!; stopped";
    $cache.print(%dict_hash.yaml);
#    $cache.print(%dict_hash.perl);
    say "Wrote $cache";
    close ($cache) or die "Closing $cache";
  }
}

say "Word list hath ", %dict_hash.elems, " pairs";
#say %dict_hash.yaml;
for (%dict_hash.keys) -> $bag {
                          my @words = %dict_hash{$bag};
                               push @dict, [$bag, @words];
                         }

1;
