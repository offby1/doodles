module Main where
import Bag

flubber :: [String] -> [(Integer, String)]
flubber lines =
        map (\w -> (make_bag (w), w)) lines

main= do
      x <- readFile ("words")
      print (flubber (lines(x)))
