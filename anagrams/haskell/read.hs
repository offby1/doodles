module Main where
import Bag

main= do
      x <- readFile ("words")
      print (map (\w -> "golly" : [w]) (lines(x)))
