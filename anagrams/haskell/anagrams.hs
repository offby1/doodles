module Main where
import Bag
import Char
import qualified Data.Map as M

type DictMap = M.Map Integer [String]
type Dict = [(Integer, [String])]

member :: String -> [String] -> Bool
member string [] = False
member string (x:xs) =
      if string == x then True else member string xs

adjoin :: Integer -> String -> DictMap -> DictMap
adjoin key string dict =
       M.insertWith (\new -> (\old -> ( 
                    if member (head new) old then old else (new ++ old) ))) key [string] dict
           

from_strings :: [String] -> DictMap
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

prune :: Integer -> Dict -> Dict
prune bag [] = []
prune bag (x:xs) = if (subtract_bags bag (fst x) ) > 0 then (x : (prune bag xs)) else prune bag xs
          
combine :: [String] -> [[String]] -> [[String]]
combine words anagrams = 
        concatMap (\w -> (map (\an -> w : an) anagrams)) words

anagrams :: Integer -> Dict -> [[String]]
anagrams bag [] = []
anagrams bag (x:xs) =
         let smaller = subtract_bags bag (fst x) in
         if (smaller > 0) then
             (if (Bag.empty smaller) then
                                map (\item -> [item]) (snd x)
                                else
                                    combine (snd x) (anagrams smaller (prune smaller (x:xs))))
                  ++ anagrams bag xs
         else
            anagrams bag xs

main= do
      x <- readFile ("/usr/share/dict/words")
      let dict = M.toList (from_strings (filter acceptable (map (map toLower) (lines x))))
          in let b = make_bag ("Ernest Hemingway")
              in let answer = anagrams (b) (prune b dict)
                 in print [show (length answer), "anagrams"]
