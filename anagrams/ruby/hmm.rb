#!/usr/bin/ruby -s
puts "Oh yeah: ", 2**64

h = { 'dog' => 'canine', 'cat' => 'feline', 'donkey' => 'asinine' }
puts h.length 	; 	3
puts h['dog'] 	; 	"canine"
h['cow'] = 'bovine'
h[12]    = 'dodecine'
h['cat'] = 99
puts h.to_s

def bob ()
  puts "bob here"
end

puts "Let's try it:"
bob
puts "Did it work?"

def assert (arg)
  if (!arg)
    raise "Uh oh."
  end
end

puts "One"
assert 1
puts "Zero"
assert 0
puts "We shouldn't have gotten here."
