require 'bag'

# First snarf the dictionary and do as much pre-processing as we can

class Dict

  # TODO -- see if we can cache the dictionary once we've read it, so
  # that subsequent calls will read that cache, presumably faster than
  # reading the original dictionary.

  File.open("/usr/share/dict/words", "r") do |aFile|
    @Anagrams_by_number = {}
    printf "Snarfing dictionary ..."
    has_a_vowel_re = /[aeiou]/
    long_enough_re = /^(..|i|a)/
    has_a_non_letter_re = /[^a-z]/
    aFile.each_line do |aLine|
      aLine.chomp!()
      aLine.downcase!()
      next if has_a_non_letter_re.match(aLine)
      next if !has_a_vowel_re.match(aLine)
      next if !long_enough_re.match(aLine)
      b = Bag.new(aLine)
      hit = @Anagrams_by_number[b]
      if (hit)
        hit = hit | [aLine]     # avoid duplicates
      else
        @Anagrams_by_number[b] = [aLine]
      end

    end
    puts " (#{@Anagrams_by_number.length} slots) done"
  end

  def Dict.Lookup(string)
    @Anagrams_by_number[Bag.new(string)]
  end

  def Dict.Prune(string)
    max = Bag.new(string)
    @Anagrams_by_number.delete_if {
      | bag, ignored |
      not (max - bag)
    }
    puts "After pruning to `#{string}', (#{@Anagrams_by_number.length} slots: "
    @Anagrams_by_number.each {
      |key, value|
      printf "Key: #{key}";
      value.each {
        |v|
        printf " #{v}"
      }
      puts ""
    }
  end
end
