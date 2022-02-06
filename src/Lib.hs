module Lib
  ( getBestGuess,
    readHint,
    Hint,
  )
where

import Control.Applicative
import Data.List (elemIndices, sortBy)
import Data.Maybe (fromJust)

alphabet :: [Char]
alphabet = ['a' .. 'z']

type Index = Int

data Hint = Green Index Char | Yellow Index Char | Red Char deriving (Show)

readHint :: (String, Index) -> Maybe Hint
readHint ('G' : xs, pos) = Just (Green pos (head xs))
readHint ('Y' : xs, pos) = Just (Yellow pos (head xs))
readHint ('R' : xs, pos) = Just (Red (head xs))
readHint _ = Nothing

hintPredicate :: Hint -> (String -> Bool)
hintPredicate (Green i c) = elem i . elemIndices c
hintPredicate (Yellow i c) = notElem i . elemIndices c
hintPredicate (Red c) = notElem c

filterWordList :: [Hint] -> [String] -> [String]
filterWordList hints = filter (\w -> all ((== True) . ($w)) filters)
  where
    filters = map hintPredicate hints

getBestGuess :: [Hint] -> [String] -> (String, [String])
getBestGuess hints wordList =
  ( fst $
      foldl
        ( \(bestWord, highScore) word ->
            if wordToScore word > highScore
              then (word, wordToScore word)
              else (bestWord, highScore)
        )
        ("", 0.0)
        uniqueLetterWordList,
    hintFilteredWordList
  )
  where
    wordToScore = foldl (\acc char -> ((+ acc) . fromJust . lookup char . letterFrequency) wordList) 0.0
    uniqueLetterWordList = if not (any uniqueCheck hintFilteredWordList) then hintFilteredWordList else filter uniqueCheck hintFilteredWordList
    hintFilteredWordList = filterWordList hints wordList

letterFrequency :: [String] -> [(Char, Double)]
letterFrequency wordList = map (\(l, o) -> (l, (o / nbrWords) * 100.0)) letterOccurance
  where
    nbrWords = fromIntegral $ length wordList
    letterOccurance = map (\l -> (l, foldl (\acc word -> if l `elem` word then acc + 1 else acc) 0 wordList)) alphabet

uniqueCheck :: String -> Bool
uniqueCheck word = uniqueCheck' word (tail word)
  where
    uniqueCheck' (x : xs) (y : ys)
      | x /= y = uniqueCheck' (x : xs) ys
      | otherwise = False
    uniqueCheck' (x : xs) [] = uniqueCheck' xs (tail xs)
    uniqueCheck' [] _ = True
