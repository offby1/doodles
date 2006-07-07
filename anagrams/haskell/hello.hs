-- -*-haskell-*-

module Main where

fact :: Integer -> Integer
fact x = case x of
           0 -> 1
           x -> x * fact (x - 1)

main = do 
  print "Hello, world "
  print (fact (100))
