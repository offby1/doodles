module Main where
import Bag
import qualified Data.Map as Map

flubber :: [String] -> [(Integer, String)]
flubber lines =
        map (\w -> (make_bag (w), w)) lines

znork :: [(Integer, String)] -> Map.Map Integer String
znork pairs =
      Map.fromList pairs 

--prepend :: a -> [a] -> [a]
prepend item [] = [item]
prepend item [xs]
        | item `elem` xs = xs
        | True = item:xs

main= do
      x <- readFile ("words")
      let dict = znork (flubber (lines (x)))
          in print (dict Map.! 710)
