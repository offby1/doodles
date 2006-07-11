module Main where
import Bag
import qualified Data.Map as Map

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
      let dict = Map.fromList (flubber (lines (x)))
          in print (dict Map.! 710)
