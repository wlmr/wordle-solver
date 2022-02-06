module Main where

import Control.Monad
import Data.Maybe (fromJust)
import Lib (getBestGuess, readHint)
import System.Environment (getArgs)
import System.IO

wordsFileName :: String
wordsFileName = "words_5_better.txt"

main :: IO ()
main = do
  contents <- readFile wordsFileName
  let wordList = words contents
  putStrLn $ "Welcome. Start by guessing " ++ fst (getBestGuess [] wordList)
  loop wordList

loop :: [String] -> IO ()
loop wordList = do
  hintsText <- getLine
  let hints = zipWith (curry (fromJust . readHint)) (words hintsText) [0 .. 4]
      (suggestion, newWordList) = getBestGuess hints wordList
  putStrLn $ "I suggest : " ++ suggestion
  loop newWordList