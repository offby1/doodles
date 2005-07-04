#!/usr/local/src/pugs/pugs

use bag;

our @dict;
our %dict_hash;
#my $dict_file_name = "/usr/share/dict/words";
my $dict_file_name = "words";

sub acceptable (Str $word) returns Bool {
  warn "Is $word acceptable?";

  if ($word ~~ m:perl5/[^[:alpha:]]/) {
     warn "False because it contains non-alpha";
     return bool::false ;
  }

  if ($word ~~ "i") {
     warn "true because it is 'i'";
    return bool::true ;
  }

  if ($word ~~ "a") {
     warn "true because it is 'a'";
    return bool::true ;
  }

if ($word ~~ m:perl5/^.$/) {
     warn "False because it is just one letter";
  return bool::false ;
}
  if ($word ~~ m:perl5/[aeiou]/) {
     warn "true because it contains a vowel";
    return bool::true ;
    }
  
    warn "False just because.";
    return bool::false;
}

sub snarf_wordlist {
  warn "ya!";

  my $dict = open($dict_file_name, :r)
    or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

  warn "Reading $dict_file_name ...";

  for ($dict.readline) -> $word {
                                 my $chopped = chomp($word);
                                 next unless (acceptable($chopped));
                                 push @{%dict_hash{&bag($chopped)}}, $chopped;
                                }
}

warn "huh";
snarf_wordlist();
print %dict_hash.perl, "\n";
warn "Gosh";

1;
