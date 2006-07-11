module Main where
import Bag
import Char
import qualified Data.Map as M

type Dict = M.Map Integer [String]

member :: String -> [String] -> Bool
member string [] = False
member string (x:xs) =
      if string == x then True else member string xs

adjoin :: Integer -> String -> Dict -> Dict
adjoin key string dict =
       M.insertWith (\new -> (\old -> ( 
                    if member (head new) old then old else (new ++ old) ))) key [string] dict
           

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
            && all isAscii word
            && hasVowel word
            && ((length (word) > 1)
               || word == "a"
               || word == "i")

main= do
      x <- readFile ("/usr/share/dict/words")
      let dict = from_strings (filter acceptable (map (map toLower) (lines x)))
          in do
             print ("words: "        , (sum (map length (M.elems dict))))
             print ("distinct bags: ", length (M.elems dict))
             --print dict
             print (dict M.! make_bag ("steal"))
             print (dict M.! make_bag ("dog"))
             print (dict M.! 18)
