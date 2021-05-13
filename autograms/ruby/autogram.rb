require 'set'
require 'pp'

# "gem install" these
require 'pluralizer'
require 'spell_number'

def histogram(string)
  lettermap = Hash.new{|h,k|h[k] = 0}
  string.chars.each do |c|
    lettermap[c.to_sym] += 1
  end

  lettermap
end

def update_histogram(template, letters)
  expanded = template % letters
end

template = "This sentence has %{a} a's and %{q} qs."
h = histogram('')

seen = Set[]
loop {
  rendered = template % h
  break if seen === rendered
  seen.add(rendered)
  puts rendered
  h = histogram(rendered)
}
