#!/usr/bin/ruby -s

require 'test/unit'

class Bag
  def initialize (str)
    @str = str.split(//).sort().join()
  end
  def empty
    0 == @str.size()
  end
  def ==(other)
    @str == other.str
  end
  def -(other)
    Bag.new("a")
  end
  protected
  def str
    @str
  end
end

class Bag_Tests < Test::Unit::TestCase
  def test_joe_bob
    assert(Bag.new("").empty(), "initially empty")
    assert((not (Bag.new("a").empty())), "not always empty")
    assert_equal(Bag.new("abc"), Bag.new("cba"))
    assert((not(Bag.new("abc") == Bag.new("bc"))))
    assert_equal(Bag.new("a"),
                 Bag.new("ab") - Bag.new("b"))
    assert_equal(Bag.new("b"),
                 Bag.new("ab") - Bag.new("a"))
  end
end
