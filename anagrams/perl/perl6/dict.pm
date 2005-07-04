#!/usr/local/src/pugs/pugs

use bag;

our @dict;
our %dict_hash;
#my $dict_file_name = "/usr/share/dict/words";
my $dict_file_name = "words";
my $dict_hash_name = "bigus-dictus.pl";

sub maybe_snarf_wordlist {
  warn "ya!";

  if (!-r $dict_hash_name) {

    my $hash = open($dict_hash_name, :w)
      or die "Oh hell, can't write to $dict_hash_name: $!";

    my $dict = open($dict_file_name, :r)
      or die "Can't open $dict_file_name for reading 'cuz $!; stopped";

    warn "Reading $dict_file_name ...";

    for ($dict.readline) -> $word {
      my $chopped = chomp($word);
      %dict_hash{&bag($word)} = $word;
    }
  }
}

warn "huh";
maybe_snarf_wordlist();
print %dict_hash;
warn "Gosh";

1;
