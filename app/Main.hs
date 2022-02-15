module Main where

import Control.Monad
import Data.Maybe (fromJust)
import Solver (getBestGuesses, readHint, filterWordList, entropy, stringToHints, Hint)
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
  putStrLn "Try one of the following:" 
  print $ getBestGuesses 10 entropy wordList
  hints <- getInput
  loop $ filterWordList hints wordList

getInput :: IO [Hint]
getInput = do 
  hintText <- getLine
  let
    hints = stringToHints hintText
  if null hints 
    then do
      putStrLn $ "Couldn't parse " ++ hintText ++ ", try again"
      getInput
    else do
      return hints