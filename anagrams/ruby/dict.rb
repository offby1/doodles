require 'bag'

# First snarf the dictionary and do as much pre-processing as we can

class Dict
  File.open("/usr/share/dict/words", "r") do |aFile|
    @Anagrams_by_number = {}
    printf "Snarfing dictionary ..."
    aFile.each_line do |aLine|
      aLine.chomp!()
      aLine.downcase!()

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
