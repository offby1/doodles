module Main where
import Bag
import qualified Data.Map as M

type Dict = M.Map Integer [String]

adjoin :: Integer -> String -> Dict -> Dict
adjoin key datum dict =
       M.insertWith (\old -> (\new -> ( new ++ old ))) key [datum] dict
           

from_strings :: [String] -> Dict
from_strings lines =
        let dict = M.empty
            in last (map (\w -> (adjoin (make_bag (w)) w dict)) lines)

main= do
      x <- readFile ("words")
      let dict = from_strings (lines (x))
          in do
             print (dict);
             print (dict M.! 710)



