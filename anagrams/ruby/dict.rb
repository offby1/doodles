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
      if (@Anagrams_by_number.has_key?(b))
        @Anagrams_by_number[b] = @Anagrams_by_number[b] | [aLine]     # avoid duplicates
      else
        @Anagrams_by_number[b] = [aLine]
      end

    end
    puts " (#{@Anagrams_by_number.length} slots) done"
  end

  def Dict.Lookup(string)
    @Anagrams_by_number[Bag.new(string)]
  end

  def Dict.Prune(max)
    result = []
    @Anagrams_by_number.each {
      | bag, words |
      if (max - bag)
        result.push([bag, words])
      end
    }

    puts "After pruning to `#{max}', (#{result.length} slots: "
    result
  end
end
