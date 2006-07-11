-- -*-haskell-*-

module Bag (make_bag, subtract_bags, empty, bags_equal) where
import Char

make_bag :: String -> Integer
make_bag []     = 1
make_bag (c:cs) = char_prime (c) * make_bag cs

-- thanks to "Cale" (Cale Gibbard
-- (i=foobar@London-HSE-ppp3543475.sympatico.ca)) for "primes"
primes = sieve [2..]; sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

char_prime :: Char -> Integer
char_prime c | (lc >= 'a') && (lc <= 'z') = primes !! (ord (lc) - ord('a'))
             | True = 1
             where lc = toLower (c)

subtract_bags :: Integer -> Integer -> Integer
subtract_bags top bot = let r = rem top bot in
                            if (r == 0) then quot top bot else 0

-- to test this, start ghci, and type
-- :load bag.hs
--  print "Hello, world "
--  print (make_bag ("Hello, world"))

-- that last should print 828806486967613

-- subtract_bags (make_bag ("ola")) (make_bag ("lo")) => 2

empty b = (b == 1)
bags_equal b1 b2 = (b1 == b2)
