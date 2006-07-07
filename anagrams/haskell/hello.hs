module Main where

fact :: Integer -> Integer
fact x = if x == 0 then 1 else x * fact (x - 1)

main = do 
     print "Hello, world "
     print (fact (100))
