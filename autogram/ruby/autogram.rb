#!/usr/bin/env ruby1.9

def histogram(string)
  lettermap = Hash.new{|h,k|h[k] = 0}
  string.chars.each do |c|
    lettermap[c] += 1
  end
  lettermap
end

def expand_template(letters, template)
  histogram(template)
end

def update_histogram(letters, template)
  next_histogram = expand_template(letters, template)

  letters.each do |letter, count|
    puts "#{letter}: #{count} => #{next_histogram[letter]}"
  end
end

h = histogram("This sentence has yadda yadda.")
update_histogram(h, "This sentence has yadda yadda.")
