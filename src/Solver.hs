module Solver
  ( getBestGuesses,
    readHint,
    Hint(..),
    Guess,
    showHint,
    guessToHint,
    correctReply,
    entropy,
    filterWordList
  )
where

import Control.Applicative
import Data.List (elemIndices, sortBy, sortOn)
import Data.Maybe (fromJust)
import Words
import qualified Data.Ord

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

guessToRed :: (Char, b) -> Hint
guessToRed (c,_) = Red c
guessToGreen :: (Char, Index) -> Hint
guessToGreen (c,i) = Green c i
guessToYellow :: (Char, Index) -> Hint
guessToYellow (c, i) = Yellow c i

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

getBestGuesses :: Int -> (String -> [String] -> Double) -> [String] -> [(String, Double)]
getBestGuesses nbrOfGuesses scoreFunc wordList = (take nbrOfGuesses . sortOn (Data.Ord.Down . snd) . map (\word -> (word, scoreFunc word wordList))) wordList

entropy :: String -> [String] -> Double
entropy word wordList = (negate . sum . map (\x -> p x * logBase 2 (p x)). filter (\hint -> nbrWordsFiltered hint /= 0.0)) (allHintCombos word)
  where p x = nbrWordsFiltered x / nbrWordsTot
        nbrWordsTot = fromIntegral $ length wordList
        nbrWordsFiltered hint = fromIntegral $ length $ filterWordList hint wordList

allHintCombos :: String -> [[Hint]]
allHintCombos word = foldr genCombos [[]] guesses
  where genCombos guess combos = concatMap (\combo -> map (\toHint -> toHint guess:combo) [guessToGreen, guessToYellow, guessToRed]) combos
        guesses = zip word [0 .. (length word - 1)]
