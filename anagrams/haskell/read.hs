module Main where
import Bag
import qualified Data.Map as Map

flubber :: [String] -> Map.Map Integer String
flubber lines =
        -- BUGBUG -- data needs to be a _list_ of strings, not a single string.
        let dict = Map.empty
            in Map.fromList (map (\w -> (make_bag (w), w)) lines)

--prepend :: a -> [a] -> [a]
prepend item [] = [item]
prepend item [xs]
        | item `elem` xs = xs
        | True = item:xs

main= do
      x <- readFile ("words")
      let dict = flubber (lines (x))
          in do
             print (dict);
             print (dict Map.! 710)
