module Main where

main= do
      x <- readFile ("/etc/passwd")
      print (lines(x))
