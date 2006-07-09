-- -*-haskell-*-

module Bag where

make_bag :: String -> Integer
make_bag []     = 1
make_bag (c:cs) = char_prime (c) * make_bag cs

char_prime :: Char -> Integer
char_prime c = case c of
           'a' -> 2
           'b' -> 3
           'c' -> 5
           'd' -> 7
           'e' -> 11
           'f' -> 13
           'g' -> 17
           'h' -> 19
           'i' -> 23
           'j' -> 29
           'k' -> 31
           'l' -> 37
           'm' -> 41
           'n' -> 43
           'o' -> 47
           'p' -> 53
           'q' -> 59
           'r' -> 61
           's' -> 67
           't' -> 71
           'u' -> 73
           'v' -> 79
           'w' -> 83
           'x' -> 89
           'y' -> 97
           'z' -> 101
           _-> 1

-- to test this, start ghci, and type
-- :load bag.hs
--  print "Hello, world "
--  print (make_bag ("Hello, world"))
