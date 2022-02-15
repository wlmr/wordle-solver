module Main where

import Control.Monad
import Data.Maybe (fromJust)
import Solver (getBestGuesses, readHint, filterWordList, entropy)
import System.Environment (getArgs)
import System.IO
import Words (answers)

main :: IO ()
main = loop answers

{-
1. get best guesses
2. input hints
3. parse hints
4. filter wordList on hints
5. repeat
-}
loop :: [String] -> IO ()
loop wordList = do
  print "Try one of the following:\n" 
  print $ getBestGuesses 10 entropy wordList
  hintText <- getLine
  let
    hintTexts = words hintText 
    indices = [0 .. (length hintTexts - 1)]
    hints = zipWith (curry (fromJust . readHint)) hintTexts indices
  loop $ filterWordList hints wordList

