module Solver
  ( getBestGuess,
    readHint,
    Hint(..),
    Guess,
    showHint,
    guessToHint,
    correctReply
  )
where

import Control.Applicative
import Data.List (elemIndices, sortBy)
import Data.Maybe (fromJust)

alphabet :: [Char]
alphabet = ['a' .. 'z']

type Index = Int
type Guess = (Char, Index)
data Hint = Green Char Index | Yellow Char Index | Red Char deriving (Show)

guessToHint :: String -> Guess -> Hint
guessToHint answer (c,i)
  | c `notElem` answer = Red c
  | i `elem` elemIndices c answer = Green c i
  | otherwise = Yellow c i

readHint :: (String, Index) -> Maybe Hint
readHint ('G' : xs, pos) = Just (Green  (head xs) pos) 
readHint ('Y' : xs, pos) = Just (Yellow (head xs) pos) 
readHint ([x]     , pos) = Just (Red x)
readHint _ = Nothing

showHint :: Hint -> String
showHint (Green  c i) = 'G':[c]
showHint (Yellow c i) = 'Y':[c]
showHint (Red    c)   =     [c]

correctGuess :: Hint -> Bool
correctGuess (Green _ _) = True
correctGuess _           = False

correctReply :: [Hint] -> Bool
correctReply = all correctGuess
  

hintPredicate :: Hint -> (String -> Bool)
hintPredicate (Green c i) = elem i . elemIndices c
hintPredicate (Yellow c i) = (\indices -> not (null indices) && (i `notElem` indices)) . elemIndices c
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
