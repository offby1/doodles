require 'bag'

# First snarf the dictionary and do as much pre-processing as we can

def Read(fn)
  begin
    File.open("hash.cache", "r") do |aCache|
      printf "Snarfing hash.cache ..."
      $stdout.flush
      @Anagrams_by_number = Marshal.load(aCache)
      puts "Loaded dictionary from hash.cache"
    end

  rescue Errno::ENOENT
    File.open(fn, "r") do |aFile|
      @Anagrams_by_number = {}
      printf "Snarfing #{fn} ..."
      $stdout.flush
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
    File.open("hash.cache", "w") do |aCache|
      Marshal.dump(@Anagrams_by_number, aCache)
      puts "Wrote hash.cache"
    end
  end
end

def Prune(max)
  result = []
  @Anagrams_by_number.each {
    | bag, words |
    if (max - bag)
      result.push([bag, words])
    end
  }

  result
end

