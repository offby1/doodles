#!/usr/local/src/langs/pugs/pugs
use v6;
use dict;
use bag;

sub combine (@words, @anagrams) {
  my @rv = ();

  for (@words) -> $w {
    push @rv, map { [$w, @$_] }, @anagrams;
  }

  @rv;
}

#say combine(["foo", "bar"], (["one", "anagram"], ["another", "gram"])).yaml;

sub anagrams (Int $bag, Int $l, @dict) {
  my @rv = [];

  loop (;
        @dict;
        @dict.shift) {
    my $first = @dict[0];
    my @rest = @dict[1..*];
    my Int $key   = $first[0];
    my @words = $first[1];

    say (First => $first, Bag => $bag, Key => $key).yaml;
    my $smaller_bag = Bag::subtract_bags($bag, $key);
    next unless ($smaller_bag > 0);
    die "Uh oh: $smaller_bag isn't < $bag" unless $smaller_bag < $bag;
    my @combined;
    if (Bag::bag_empty ($smaller_bag)) {
      @combined = map { [$_]  }, @words;
    } else {
      my @from_smaller_bag = anagrams($smaller_bag,
                                      $l + 1,
                                      @rest);
      next unless (@from_smaller_bag);

      @combined = combine(@words, @from_smaller_bag);
    }
    push @rv, @combined;
    say (combined => @combined.yaml) if (!$l);
  }

  return @rv;
}

my $cat = Bag::bag("cat");
say (cat => $cat);
say (finally => anagrams($cat, 0, @dict::dict));

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
