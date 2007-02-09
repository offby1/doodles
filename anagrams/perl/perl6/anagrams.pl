#!/usr/local/src/langs/pugs/pugs
use v6;
use dict;
use bag;

sub combine (@words, @anagrams) {
  my @rv = ();

  for (@words) -> $w {
    push @rv, map { [$w, @$_] }, @anagrams;
  }

  say "words";
  say @words.perl;
  say "anagrams";
  say @anagrams.perl;
  say "yields";
  say @rv.perl;
  @rv;
}

#say combine(["foo", "bar"], (["one", "anagram"], ["another", "gram"])).perl;

sub anagrams (Int $bag, Int $l, @dict) {
  my @rv = ();

  loop (my $processed = 0;
        $processed < @dict;
        $processed++) {
    my $first = @dict[$processed];
    my @rest = @dict[$processed + 1..*];
    my Int $key   = $first[0];
    my $words = $first[1];

    say (First => $first.perl, Bag => $bag.perl, Key => $key.perl);
    my $smaller_bag = Bag::subtract_bags($bag, $key);
    next unless ($smaller_bag > 0);
    die "Uh oh: $smaller_bag isn't < $bag" unless $smaller_bag < $bag;
    my @combined;
    if (Bag::bag_empty ($smaller_bag)) {
      @combined = map { [$_]  }, @$words;
    } else {
      my @from_smaller_bag = anagrams($smaller_bag,
                                      $l + 1,
                                      @rest);
      next unless (@from_smaller_bag);
      say "This is supposedly a non-empty list of anagrams from ",
        (smaller_bag => $smaller_bag.perl),
          (from_smaller_bag => @from_smaller_bag.perl);

      @combined = combine($words, @from_smaller_bag);
    }
    push @rv, @combined;
    if (!$l) {
      say "combined";
      say @combined.perl;
    }
  }

  return @rv;
}

my $cat = Bag::bag("cat");
say (cat => $cat);
say (finally => anagrams($cat, 0, @dict::dict));
say "And now, dog:";
say (dog => anagrams(Bag::bag("dog"), 0, @dict::dict).perl);

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
