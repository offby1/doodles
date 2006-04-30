#!/usr/local/src/langs/pugs/pugs

use bag;

our @dict;
our %dict_hash;
#my $dict_file_name = "/usr/share/dict/words";
my $dict_file_name = "words";

sub acceptable (Str $word) returns Bool {
  print $*ERR: "Is '$word' acceptable? ... ";

  if ($word ~~ m:perl5/[^[:alpha:]]/) {
     print $*ERR: "False because it contains non-alpha\n";
     return bool::false ;
  }

  if ($word ~~ "i") {
     print $*ERR: "true because it is 'i'\n";
    return bool::true ;
  }

  if ($word ~~ "a") {
     print $*ERR: "true because it is 'a'\n";
    return bool::true ;
  }

  if ($word ~~ m:perl5/^.$/) {
     print $*ERR: "False because it is just one letter\n";
     return bool::false ;
    }

  if ($word ~~ m:perl5/[aeiou]/) {
     print $*ERR: "true because it contains a vowel\n";
    return bool::true ;
    }

    print $*ERR: "False just because.\n";
    return bool::false;
}

sub snarf_wordlist {
  warn "ya!";

  my $dict = open($dict_file_name, :r)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  print $*ERR: "Reading $dict_file_name ...";

  for ($dict.readline) -> $word {
                                 my $chopped = chomp($word);
                                 warn "Read '$chopped'";
                                 next unless (acceptable($chopped));
                                 push @{%dict_hash{&bag($chopped)}}, $chopped;
                                };
  print $*ERR: " done\n";
}

snarf_wordlist();

1;
