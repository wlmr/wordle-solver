import Solver 

answer :: String
answer = "elder"

wordsFileName :: String
wordsFileName = "words_5_better.txt"

main :: IO ()
main = do
  contents <- readFile wordsFileName
  let wordList = words contents
  putStrLn $ "Answer: " ++ answer
  guessLoop 1 wordList []
  
guessLoop :: Int -> [String] -> [Hint] -> IO ()
guessLoop nbrGuess wordList hints = do
  let (suggestion, newWordList) = getBestGuess hints wordList
      newHints = zipWith (curry (guessToHint answer)) suggestion [0 .. 4]
  putStrLn suggestion
  putStrLn $ unwords $ map showHint newHints
  if correctReply newHints 
    then do
      putStrLn $ "Found the answer in " ++ show nbrGuess ++ " number of guesses"
    else do
      guessLoop (nbrGuess + 1) newWordList newHints


notMain = putStrLn "String"

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

