import Solver
import Data.List (sortBy, sortOn)
import qualified Data.Ord
import Words (answers)
import Solver (filterWordList)

wordsFileName :: String
wordsFileName = "words_5_better.txt"

answer = "aroma"

main :: IO ()
main = do
  putStrLn $ "Answer: " ++ answer
  loop 1 answers 


loop :: Int -> [String] -> IO ()
loop nbrGuess wordList = do
  putStr "Trying the following: " 
  let (suggestion, score):_ = getBestGuesses 1 entropy wordList
      hints = zipWith (curry (guessToHint answer)) suggestion [0 .. 4]
  putStrLn suggestion
  if correctReply hints
    then do
      putStrLn $ "Found the answer in " ++ show nbrGuess ++ " number of guesses"
    else do
      loop (nbrGuess + 1) (filterWordList hints wordList)





{- 
Game:
  let answer = String
      hints = []
      wordList = [String]
      (suggestion, newWordList) = getBestGuess hints wordList
      hints = map charToHint suggestion
-}
-- testInput = ["Gs","Re","Ra","Rr","Re"]
-- testWords = ["seren", "solon", "srakc", "oloci"]

-- testInputs :: [([Char], Int)]
-- testInputs = zip testInput [0..4]

