module Main where
import Bag

flubber :: [String] -> [(Integer, String)]
flubber lines =
        map (\w -> (make_bag (w), w)) lines

--prepend :: a -> [a] -> [a]
prepend item [] = [item]
prepend item [xs]
        | item `elem` xs = xs
        | True = item:xs

main= do
      x <- readFile ("words")
      print (flubber (lines(x)))
