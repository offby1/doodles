module Main where
import Bag

main= do
      x <- readFile ("words")
      print (map (\w -> (make_bag (w), w))  (lines(x)))
