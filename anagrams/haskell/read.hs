module Main where

main= do
      x <- readFile ("words")
      print (map (\w -> "golly" : [w]) (lines(x)))
