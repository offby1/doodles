require 'set'
require 'pp'

require 'active_support/core_ext/string/inflections' # gem install rails
require 'spell_number'                               # gem install spell_number
require 'ruby-progressbar'      # gem install ruby-progressbar


def histogram(string)
  lettermap = Hash.new(0)
  string.chars.each do |c|
    lettermap[c] += 1
  end

  lettermap
end

def randomize_histogram(h, max)
  new = Hash.new(0)
  h.each {
    |k, v|

    new[k] = Random.rand(max)
  }
  new
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

template = ("This sentence has %{a}, %{b}, %{c}, %{d}, %{e}, %{f}, %{g}, %{h}, %{i}, %{j}, %{k}, %{l}, %{m}, " +
            "%{n}, %{o}, %{p}, %{q}, %{r}, %{s}, %{t}, %{u}, %{v}, %{w}, %{x}, %{y}, and %{z}.")

h = histogram(template)

seen = Set[]
last = nil
progressbar = ProgressBar.create(
  :total => nil,
  :title => "Attempts",
  :format => "%t: %c; %a",
)
loop {
  rendered = render(template, h)
  if last == rendered
    puts "Hooray, truth"
    puts rendered
    break
  end
  last = rendered
  if seen === rendered
    h = randomize_histogram(h, rendered.length())
  else
    seen.add(rendered)
    h = histogram(rendered)

    progressbar.increment
  end
}
