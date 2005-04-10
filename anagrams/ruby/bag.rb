require 'test/unit'

class Bag
  Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]

  def initialize (str)
    @product = 1
    str.downcase().each_byte {
      |b|
      if (b >= ?a and b <= ?z)
        @product *= Primes[b - ?a]
      end
    }
  end

  def hash
    @product.hash
  end
  
  def empty
    1 == @product
  end

  def ==(other)
    @product == other.product
  end

  def eql?(other)
    @product == other.product
  end

  def -(other)
    if(@product % other.product != 0)
      nil
    else
      p = Bag.new("")
      p.product = @product / other.product
      p
    end
  end

  def product
    @product
  end

  protected

  def product=(x)
    @product = x
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
    assert((not( Bag.new("a") - Bag.new("b"))))
    assert((not( Bag.new("a") - Bag.new("aa"))))
    silly_long_string = "When first I was a wee, wee lad\n\
Eating on my horse\n\
I had to take a farting duck\n\
Much to my remorse.\n\
Oh Sally can't you hear my plea\n\
When Roe V Wade is nigh\n\
And candles slide the misty morn\n\
With dimples on your tie."

    ever_so_slightly_longer_string = silly_long_string + "x"
    assert_equal(Bag.new("x"),
                 Bag.new(ever_so_slightly_longer_string) - Bag.new(silly_long_string))
    
    assert_equal(Bag.new("abc"),
                 Bag.new("ABC"), "properly insensitive to case")
    
  end
end
