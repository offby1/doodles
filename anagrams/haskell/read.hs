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

isVowel :: Char -> Bool
isVowel c = 
        any (\v -> v == c) "aeiou" 

hasVowel :: String -> Bool
hasVowel s =
         any isVowel s

acceptable :: String -> Bool
acceptable word =
           all isAlpha word
            && hasVowel word
            && ((length (word) > 1)
               || word == "a"
               || word == "i")          

main= do
      x <- readFile ("/usr/share/dict/words")
      let dict = from_strings (filter acceptable (map (map toLower) (lines x)))
          in do
             print (sum (map length (M.elems dict)))
             print (take 3 (M.toList dict));
             print (dict M.! make_bag ("steal"))
