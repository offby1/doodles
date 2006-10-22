#!/usr/local/src/langs/pugs/pugs

use bag;

our @dict;
our %dict_hash;
#my $dict_file_name = "/usr/share/dict/words";
my $dict_file_name = "words";

sub acceptable (Str $word) returns Bool {
  print $*ERR: "Is '$word' acceptable? ... ";

  if ($word ~~ rx:perl5{[^[:alpha:]]}) {
    print $*ERR: "False because it contains non-alpha\n";
    return Bool::False ;
  }

  if ($word ~~ "i") {
    print $*ERR: "true because it is 'i'\n";
    return Bool::True ;
  }

  if ($word ~~ "a") {
    print $*ERR: "true because it is 'a'\n";
    return Bool::True ;
  }

  if ($word.chars < 2) {
    print $*ERR: "False because it is just one letter\n";
    return Bool::False ;
  }

  if ($word ~~ rx:perl5{[aeiou]}) {
    print $*ERR: "true because it contains a vowel\n";
    return Bool::True ;
  }

  print $*ERR: "False just because.\n";
  return Bool::False;
}

sub snarf_wordlist {
  my $dict = open($dict_file_name, :r)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  print $*ERR: "Reading $dict_file_name ...";

  for ($dict.readline) -> $word {
                                 my $chopped = chomp($word);
                                 warn "Read '$chopped'";
                                 $chopped = $chopped.lc;
                                 next unless (acceptable($chopped));
                                 %dict_hash{bag($chopped)}.push($chopped);
                                };
  print $*ERR: " done\n";
}

snarf_wordlist();

1;
