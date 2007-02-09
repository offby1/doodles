#!/usr/local/src/langs/pugs/pugs
use v6;
use dict <dict>;
use bag;

sub combine (@words, @anagrams) {
  my @rv = ();

  for (@words) -> $w {
    push @rv, map { [$w, @$_] }, @anagrams;
  }

  @rv;
}

say combine(["foo", "bar"], (["one", "anagram"], ["another", "gram"])).yaml;

sub anagrams ($bag, $l, @dict) {
  say "anagrams:";
  say ($bag.yaml, @dict.yaml);
  my @rv = [];

  while (@dict) {
    my $first = @dict[0];
    my @rest = @dict[1..*];
    my $key   = @first[0];
    my @words = @first[1];

    my $smaller_bag = subtract_bags ($bag, $key);
    next unless ($smaller_bag);

    my @combined;
    if (bag_empty ($smaller_bag)) {
      @combined = map { [$_]  }, @words;
    } else {
      my @from_smaller_bag = anagrams ($smaller_bag,
                                       $l + 1,
                                       @rest);
      next unless (@from_smaller_bag);

      @combined = combine (@words, @from_smaller_bag);
    }
    push @rv, @combined;
    say @combined.yaml if (!$l);

    @dict.pop;
  }

  return @rv;
}

say "Hey!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
my $cat = Bag::bag("cat");
say $cat;
say @Dict::dict.yaml;
die "Outta here";
say anagrams($cat, 0, @Dict::dict);

# {
#   my $input = shift;
#   die "Say something!" unless defined ($input);
#   my $input_as_bag = bag ($input);
#   init ($input_as_bag);

#   my $result = anagrams ($input_as_bag, 0, @dict);
#   print STDERR scalar (@$result),
#     " anagrams of $input\n";
#   print join (' ', map { "(" . join (' ', @$_) . ")" } @$result),
#         "\n";
# }
