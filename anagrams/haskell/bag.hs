-- -*-haskell-*-

module Bag (make_bag) where
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

-- to test this, start ghci, and type
-- :load bag.hs
--  print "Hello, world "
--  print (make_bag ("Hello, world"))

-- that last should print 828806486967613
