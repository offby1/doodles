require 'set'
require 'pp'

require 'active_support/core_ext/string/inflections' # gem install rails
require 'spell_number'# gem install spell_number

def histogram(string)
  lettermap = Hash.new(0)
  string.chars.each do |c|
    lettermap[c] += 1
  end

  lettermap
end

def update_histogram(template, letters)
  expanded = template % letters
end

def render(template, histogram)
  new = Hash.new(0)
  histogram.each {
    |k, v|

    k = 'space' if k == ' '
    new[k.to_sym] = "#{SpellNumber.number_to_words(v)} #{k.pluralize(v)}"
  }

  template % new
end

template = "This sentence has %{a} and %{q}."
h = histogram(template)

seen = Set[]
loop {
  rendered = render(template, h)
  break if seen === rendered
  seen.add(rendered)
  puts rendered
  h = histogram(rendered)
}

