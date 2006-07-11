module Main where
import Bag
import Char
import qualified Data.Map as M

type Dict = M.Map Integer [String]

adjoin :: Integer -> String -> Dict -> Dict
adjoin key datum dict =
       M.insertWith (\old -> (\new -> ( new ++ old ))) key [datum] dict
           

from_strings :: [String] -> Dict
from_strings [] = M.empty
from_strings (line:lines) =
             adjoin (make_bag (line)) line (from_strings (lines))

--acceptable :: String -> Bool
--acceptable word =
           

main= do
      x <- readFile ("/usr/share/dict/words")
      let dict = from_strings (map (map toLower) (lines x))
          in do
             print (dict M.! make_bag ("steal"))
